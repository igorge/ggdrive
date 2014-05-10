package gie.ggdrive

import net.fusejna.util.FuseFilesystemAdapterFull
import net.fusejna.StructStat.StatWrapper
import java.io.{RandomAccessFile, File}
import net.fusejna.types.TypeMode.{ModeWrapper, NodeType}
import net.fusejna.StructFuseFileInfo.FileInfoWrapper
import net.fusejna.{DirectoryFiller, ErrorCodes}
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer
import scala.compat.Platform._
import scala.Some
import scala.collection.immutable.IndexedSeq
import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

class FUSEException(val errCode: Int, path:String=null) extends Exception(path)
class FUSEAccessDeniedException(path:String) extends FUSEException(-ErrorCodes.EACCES(), path)
class FUSEFileNotFoundException(path:String) extends FUSEException(-ErrorCodes.ENOENT(), path)
class FUSENotADirectoryException(path:String) extends FUSEException(-ErrorCodes.ENOTDIR(), path)

case class FUSERegularFileDescriptor(fileDesc: GFile, local:RandomAccessFile)

class FUSEDrive(drive: GDrive, gqueue: GDriveQueue) extends FuseFilesystemAdapterFull with LazyLogging {

  this.log(true)

  val OPEN_FILE_TIMEOUT = 1 minute
  val COPY_BUFFER_SIZE = 4*1024

  def withExceptionGuard(code: =>Any): Int = try {
    code
    0
  } catch {
    case e: FUSEException =>
      logger.debug(s"Returning exception '${e}' to client as error code '${e.errCode}'")
      e.errCode
    case e: Throwable =>
      logger.error(s"Exception leaked from FUSE handler: ${e}${EOL}${e.getStackTrace().mkString("", EOL, EOL)}")
      -ErrorCodes.EACCES()
  }

  def valueWithExceptionGuard(code: =>Int): Int = try {
    code
  } catch {
    case e: FUSEException =>
      logger.debug(s"Returning exception '${e}' to client as error code '${e.errCode}'")
      e.errCode
    case e: Throwable =>
      logger.error(s"Exception leaked from FUSE handler: ${e}${EOL}${e.getStackTrace().mkString("", EOL, EOL)}")
      -ErrorCodes.EACCES()
  }

  var inode:Long = 1

  def getInode() = synchronized {
    var r = inode
    inode +=1
    r
  }

  val files = mutable.Map[Long, ArrayBuffer[Byte]]()
  val names = mutable.Map[String, Long]()

  private def impl_pathToComponents(path: String): IndexedSeq[String] = {

    val pathComponents = new ArrayBuffer[String]()
    val stringBuilder = new StringBuilder()

    val iter = path.toIterator.buffered

    def isSeparator(c: Char) = (c=='\\' || c=='/')

    def skipPathSeparator(){
      while(iter.hasNext && isSeparator(iter.head)) {iter.next()}
    }

    def parseComponent() {
      stringBuilder.clear()
      skipPathSeparator()
      while(iter.hasNext && !isSeparator(iter.head)) {stringBuilder += iter.next()}
    }

    while(iter.hasNext){
      parseComponent()
      if(!stringBuilder.isEmpty) {
        val str = stringBuilder.toString()
        assume(!str.isEmpty)
        pathComponents += str
      }
    }

    pathComponents.toIndexedSeq
  }

  private def impl_locateFile(root:GDirectory, path: IndexedSeq[String]):Option[GNode] = {

    var break = false
    var current:GNode = root
    var i = 0
    while(!break && (current ne null) && i<path.size) {
      current match {
        case dir: GDirectory=>
          current = dir.threadSafeGet(path(i)).getOrElse(null)
          i += 1
        case _ => break = true
      }
    }

    Option(current)
  }

  private val handleMapper = new java.util.concurrent.ConcurrentHashMap[Long, AnyRef]()

  override def opendir(path: String, info: FileInfoWrapper): Int = withExceptionGuard {
    logger.debug(s"opendir(${path})")

    val pc = impl_pathToComponents(path)
    val fsObject = impl_locateFile(drive.root(), pc)

    fsObject.fold[Unit]{
      throw new FUSEFileNotFoundException(path)
    }{
      case dir: GDirectory=>
        assume(dir.threadSafeValid_?)
        val handleId = getInode()
        info.fh(handleId)
        val old = handleMapper.put(handleId, dir)
        assume(old eq null)

      case _ => throw new FUSENotADirectoryException(path)
    }

  }

  override def releasedir(path: String, info: FileInfoWrapper): Int = withExceptionGuard {

    logger.debug(s"releasedir(${path})")

    assume(info.fh() != 0)
    val dir = handleMapper.remove(info.fh())
    assume(dir.isInstanceOf[GDirectory])
    info.fh(0)
  }

  override def readdir(path: String, filler: DirectoryFiller): Int = withExceptionGuard {
    logger.debug(s"readdir(${path})")

    val pc = impl_pathToComponents(path)
    val fsObject = impl_locateFile(drive.root(), pc)

    fsObject.fold[Unit]{
      throw new FUSEFileNotFoundException(path)
    }{
      case dir: GDirectory=>
        dir.withReadLock {
          if(!dir.valid_?) throw new FUSEFileNotFoundException(path)
          filler.add( dir.children.toIterable.map{ _._2.name}.asJava )
        }

      case _ => throw new FUSENotADirectoryException(path)
    }


  }

  override def create(path: String, mode: ModeWrapper, info: FileInfoWrapper): Int = synchronized {
//  -ErrorCodes.EEXIST()
    -ErrorCodes.EACCES()
  }

  override def fgetattr(path: String, stat: StatWrapper, info: FileInfoWrapper): Int = withExceptionGuard {
    logger.debug(s"fgetattr(${path})")

    assume(info.fh() != 0)
    val fileDescAny = handleMapper.get(info.fh())
    assume(fileDescAny ne null)

    fileDescAny match {
      case dir: GDirectory=>
        if(!dir.threadSafeValid_?) throw new FUSEFileNotFoundException(path)
        stat.setMode(NodeType.DIRECTORY)

      case FUSERegularFileDescriptor(file, _)=>
        if(!file.threadSafeValid_?) throw new FUSEFileNotFoundException(path)
        stat.setMode(NodeType.FILE)
        stat.size(file.g.getFileSize)

      case _ => throw new UnsupportedOperationException(path)
    }

  }

  override def getattr(path: String, stat: StatWrapper): Int = withExceptionGuard {
    logger.debug(s"getattr(${path})")

    val pc = impl_pathToComponents(path)
    val fsObject = impl_locateFile(drive.root(), pc)

    fsObject.fold[Unit]{
      throw new FUSEFileNotFoundException(path)
    }{
      case dir: GDirectory=>
        if(!dir.threadSafeValid_?) throw new FUSEFileNotFoundException(path)
        stat.setMode(NodeType.DIRECTORY)
      case file: GFile=>
        if(!file.threadSafeValid_?) throw new FUSEFileNotFoundException(path)
        stat.setMode(NodeType.FILE)
        stat.size(file.withReadLock{Option(file.g.getFileSize).map(_.longValue).getOrElse(0)})

      case _ => throw new UnsupportedOperationException(path)
    }
  }

  override def open(path: String, info: FileInfoWrapper): Int = withExceptionGuard {
    logger.debug(s"open(${path})")

    val pc = impl_pathToComponents(path)
    val fsObject = impl_locateFile(drive.root(), pc)

    fsObject.fold[Unit]{
      throw new FUSEFileNotFoundException(path)
    }{
      case file: GFile =>
        assume(file.threadSafeValid_?)
        val local = Await.result(gqueue.openFile(file.id), OPEN_FILE_TIMEOUT)

        val handleId = getInode()
        info.fh(handleId)
        val old = handleMapper.put(handleId, FUSERegularFileDescriptor(file, local))
        assume(old eq null)

      case _ => throw new FUSEAccessDeniedException(path)
    }

  }

  override def release(path: String, info: FileInfoWrapper): Int = withExceptionGuard {
    logger.debug(s"release(${path})")

    assume(info.fh() != 0)
    val file = handleMapper.remove(info.fh()).asInstanceOf[FUSERegularFileDescriptor]
    info.fh(0)
    file.local.close()
  }


  override def read(path: String, buffer: ByteBuffer, size: Long, offset: Long, info: FileInfoWrapper): Int = valueWithExceptionGuard {
    logger.debug(s"[H:${info.fh()}]  read(${path}, <buffer ptr>, size:${size}, offset:${offset})")

    assume(info.fh() != 0)
    val fileDesc = handleMapper.get(info.fh()).asInstanceOf[FUSERegularFileDescriptor]
    assume(fileDesc ne null)

    assume(size > 0)
    assume(size <= Int.MaxValue)

    val tmpBuffer = new Array[Byte](size.toInt)

    fileDesc.local.synchronized {
      fileDesc.local.seek(offset)
      val r = fileDesc.local.read(tmpBuffer, 0, size.toInt)

      if (r != -1) {
        buffer.put(tmpBuffer, 0, r)
        logger.debug(">>>> " + r)
        r
      } else {
        logger.debug(">>>> EOF")
        0
      }
    }
  }

  override def write(path: String, buf: ByteBuffer, bufSize: Long, writeOffset: Long, info: FileInfoWrapper): Int = synchronized {
    assume(info.fh() != 0)
    val inode = Some(info.fh())

    if(inode.isDefined) {
      val fileData = files.getOrElseUpdate(inode.get, new ArrayBuffer[Byte])

      val atleastSize = bufSize + writeOffset

      while(fileData.size<atleastSize) {
        fileData += 0
      }

      bufSize.toInt
    } else {
      logger.debug(s"File not found: ${path}")
      -ErrorCodes.ENOENT()
    }

  }
}