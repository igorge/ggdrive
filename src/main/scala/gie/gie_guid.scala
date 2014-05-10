package gie

import java.util.{UUID=>jUUID}
import java.nio.ByteBuffer
//import scala.util.Try


class UUID(val jUuid:jUUID) extends AnyVal with Ordered[UUID] with Serializable {
  override def toString() = jUuid.toString()
  override def compare(other: UUID) = jUuid.compareTo(other.jUuid)
  def toByteArray() = ByteBuffer.wrap( Array[Byte](16) ).putLong( jUuid.getMostSignificantBits() ).putLong( jUuid.getLeastSignificantBits() ).array()
}

object UUID {
  val uuidStringSize = 36
  def unapply(data: String):Option[UUID] = if(data.size!=uuidStringSize) None else this.apply(data).toOption
  def apply() = new UUID( jUUID.randomUUID() )
  def apply(data:String) = scala.util.Try{ jUUID.fromString(data) } map { new UUID(_) }
  def apply(data: Array[Byte]) = {
    assume( data.size==16 )
    val bb = ByteBuffer.wrap(data)
    new UUID( new jUUID( bb.getLong(), bb.getLong() ) )
  }

  val zero =  new UUID( new jUUID(0,0) )
}

