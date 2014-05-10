package gie.utils

import java.io.{InputStream, OutputStream}

package object binary_io {

  @inline final val SIZE_OF_BYTE = 1
  @inline final val SIZE_OF_SHORT = 2
  @inline final val SIZE_OF_INT = 4
  @inline final val SIZE_OF_LONG = 8

  @inline final def size_of(v: Byte) = SIZE_OF_BYTE
  @inline final def size_of(v: Short) = SIZE_OF_SHORT
  @inline final def size_of(v: Int) = SIZE_OF_INT
  @inline final def size_of(v: Long) = SIZE_OF_LONG

  def writeLong(v: Long, writeBuffer: Array[Byte], idx: Int=0) = {
    writeBuffer(0+idx) = (v >>> 56).asInstanceOf[Byte]
    writeBuffer(1+idx) = (v >>> 48).asInstanceOf[Byte]
    writeBuffer(2+idx) = (v >>> 40).asInstanceOf[Byte]
    writeBuffer(3+idx) = (v >>> 32).asInstanceOf[Byte]
    writeBuffer(4+idx) = (v >>> 24).asInstanceOf[Byte]
    writeBuffer(5+idx) = (v >>> 16).asInstanceOf[Byte]
    writeBuffer(6+idx) = (v >>> 8).asInstanceOf[Byte]
    writeBuffer(7+idx) = (v >>> 0).asInstanceOf[Byte]

    v
  }

  def readLong(readBuffer: Array[Byte], idx: Int=0): Long = {
    (  (readBuffer(0+idx).asInstanceOf[Long] << 56) +
      ((readBuffer(1+idx) & 255).asInstanceOf[Long] << 48) +
      ((readBuffer(2+idx) & 255).asInstanceOf[Long] << 40) +
      ((readBuffer(3+idx) & 255).asInstanceOf[Long] << 32) +
      ((readBuffer(4+idx) & 255).asInstanceOf[Long] << 24) +
      ((readBuffer(5+idx) & 255) << 16) +
      ((readBuffer(6+idx) & 255) << 8) +
      ((readBuffer(7+idx) & 255) << 0))
  }


  def writeLong(v: Long, writeBuffer: OutputStream) {
    writeBuffer.write((v >>> 56).asInstanceOf[Byte])
    writeBuffer.write((v >>> 48).asInstanceOf[Byte])
    writeBuffer.write((v >>> 40).asInstanceOf[Byte])
    writeBuffer.write((v >>> 32).asInstanceOf[Byte])
    writeBuffer.write((v >>> 24).asInstanceOf[Byte])
    writeBuffer.write((v >>> 16).asInstanceOf[Byte])
    writeBuffer.write((v >>> 8).asInstanceOf[Byte])
    writeBuffer.write((v >>> 0).asInstanceOf[Byte])
  }

  def readLong(readBuffer: InputStream): Long = {
    (  (readBuffer.read().asInstanceOf[Long] << 56) +
      ((readBuffer.read() & 255).asInstanceOf[Long] << 48) +
      ((readBuffer.read() & 255).asInstanceOf[Long] << 40) +
      ((readBuffer.read() & 255).asInstanceOf[Long] << 32) +
      ((readBuffer.read() & 255).asInstanceOf[Long] << 24) +
      ((readBuffer.read() & 255) << 16) +
      ((readBuffer.read() & 255) << 8) +
      ((readBuffer.read() & 255) << 0))
  }
  
    
}