package gie.ggdrive

import java.io._
import gie.utils.binary_io
import java.util.zip.CRC32


package object checksum_serializer {

  val magic:Long = 0xBADF00D

  class InvalidChecksum(val checksum: Long, calculatedChecksum: Long) extends Exception {
    override def getMessage() = s"Invalid checksum, '${checksum}' != '${calculatedChecksum}'"
  }

  def sumFrom(o: AnyRef): Array[Byte] = {

    val osB = new ByteArrayOutputStream()

    binary_io.writeLong(magic, osB); osB.flush()

    val osO = new ObjectOutputStream(osB)
    osO.writeObject(o); osO.flush()

    val bin = osB.toByteArray

    val crcCalculator = new CRC32
    crcCalculator.update(bin, binary_io.SIZE_OF_LONG, bin.size-binary_io.SIZE_OF_LONG)
    val crc = crcCalculator.getValue

    assume(binary_io.readLong(bin) == magic)
    binary_io.writeLong(crc, bin, 0)

    bin
  }

  def check(data: Array[Byte]): (Boolean, Long, Long) = {
    val inBlobCrc = binary_io.readLong(data, 0)

    val crcCalculator = new CRC32
    crcCalculator.update(data, binary_io.size_of(inBlobCrc), data.size - binary_io.size_of(inBlobCrc))
    val calculatedCrc = crcCalculator.getValue

    (calculatedCrc==inBlobCrc, inBlobCrc, calculatedCrc)
  }

  def loadAndCheck(data: Array[Byte]): AnyRef = {
    val isB = new ByteArrayInputStream(data)
    val inBlobCrc = binary_io.readLong(isB)

    val crcCalculator = new CRC32
    crcCalculator.update(data, binary_io.size_of(inBlobCrc), data.size - binary_io.size_of(inBlobCrc))
    val calculatedCrc = crcCalculator.getValue

    if(inBlobCrc!=calculatedCrc) throw new InvalidChecksum(inBlobCrc, calculatedCrc)

    new ObjectInputStream(isB).readObject()
  }

}