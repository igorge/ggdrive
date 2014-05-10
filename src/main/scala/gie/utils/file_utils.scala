package gie

import java.io._
import java.nio.file.{FileVisitResult, Path, SimpleFileVisitor, Files}
import java.nio.file.attribute.BasicFileAttributes

package object file_utils {
  def getBaseFileName(file: File) = {
    val name = file.getName
    val pos = name.lastIndexOf('.')
    if(pos>=0) name.substring(0, pos) else name
  }

  def getExtension(file: File) = {
    val name = file.getName
    val pos = name.lastIndexOf('.')
    if(pos>=0) name.substring(pos) else ""
  }

  def copy(in: InputStream, out: OutputStream, bufferSize:Int = 2*1024*1024){
    assume(bufferSize>1)
    val buffer = new Array[Byte](bufferSize)
    Iterator.continually{
      in.read(buffer)
    }.takeWhile(_ != -1).foreach { size => out.write(buffer,0, size) }
  }

  def copyEx(bufferSize:Int = 2*1024*1024)(read: Array[Byte]=>Int)(write: (Array[Byte], Int, Int)=>Unit)(checkInterrupted: =>Unit){
    assume(bufferSize>1)
    val buffer = new Array[Byte](bufferSize)
    Iterator.continually{
      checkInterrupted
      read(buffer)
    }.takeWhile(_ != -1).foreach { size => write(buffer,0, size) }
  }

  def readAsArray(in: InputStream, bufferSize:Int = 2*1024*1024) = {
    val os = new ByteArrayOutputStream(bufferSize)
    copy(in, os, bufferSize)
    os.toByteArray()
  }

  def file(root: java.io.File, leaf: String) = new java.io.File(root, leaf)

  def file(path: String) = new java.io.File(path)

  object ImplicitFileOps {

    implicit class FileOps(val file: java.io.File) extends AnyVal {
      def /(leaf: String) = new File(file, leaf)

      def open_!() = new FileInputStream(file)
      def create_!() = new FileOutputStream(file)
      def deleteRecursive_!(){
        assume(file.isDirectory)
        Files.walkFileTree(file.toPath, new SimpleFileVisitor[Path]{
          override def visitFile(file: Path, attrs: BasicFileAttributes) = {
            Files.delete(file)
            FileVisitResult.CONTINUE
          }
          override def visitFileFailed(file: Path, e: IOException) = {
            Files.delete(file)
            FileVisitResult.CONTINUE
          }
          override def postVisitDirectory(file: Path, e: IOException) = {
            if(e eq null) {
              Files.delete(file)
              FileVisitResult.CONTINUE
            } else {
              throw e
            }
          }
        })
      }
    }

  }

}