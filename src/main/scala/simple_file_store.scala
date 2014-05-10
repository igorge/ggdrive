package gie.ggdrive

import java.io.{FileNotFoundException, RandomAccessFile, File}
import com.typesafe.scalalogging.slf4j.LazyLogging
import gie.{UUID, file_utils}
import gie.file_utils.ImplicitFileOps._
import scala.language.postfixOps
import scala.collection.mutable
import scala.language.postfixOps
import gie.utils.prop.Configuration
import gie.utils.ImplicitPipe._
import gie.file_utils.file
import java.nio.file.FileAlreadyExistsException


class FileStore(config: Configuration) extends ThreadLocks with LazyLogging { store =>

  type ResourceId = String

  val root = config('store_root) |> file

  val metaExt=".meta"
  val dataExt=".data"

  //private val m_metaRoot = new File(root, "meta")
  private val m_dataRoot = new File(root, "data")
  private val m_tmpRoot = new File(root, "tmp")

  {// ctor
    root.mkdirs()
    m_tmpRoot.mkdirs()
    m_dataRoot.mkdirs()

  }


  def createFile(fileId: ResourceId) = withReadLock {
    val filePath = m_dataRoot / fileId
    if(filePath.exists()) throw new FileAlreadyExistsException(filePath.toString)
    new RandomAccessFile(filePath, "rw")
  }

  def openFile(fileId: ResourceId) = withReadLock {
    val filePath = m_dataRoot / fileId
    if(!filePath.exists()) throw new FileNotFoundException(filePath.toString)
    new RandomAccessFile(filePath, "rw")
  }

  //def

}