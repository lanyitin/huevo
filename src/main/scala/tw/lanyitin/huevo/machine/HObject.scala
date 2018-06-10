package tw.lanyitin.huevo.machine

import java.nio.ByteBuffer

object HNumber {
  def apply(n: Int): HNumber = {
    HNumber(ByteBuffer.allocate(4).putInt(n))
  }

  def apply(n: Float): HNumber = {
    HNumber(ByteBuffer.allocate(4).putFloat(n))
  }
}

trait HObject {
  override def toString: String
  def byteSize: Int
}

case class HNumber(bytes: ByteBuffer) extends HObject {
  override def toString = {
    bytes.compact
    bytes.getInt.toString
  }
  def byteSize = bytes.capacity
  def getInt = {
    bytes.compact
    bytes.getInt
  }
  def putInt(value: Int) = bytes.putInt(value)
  def getFloat = bytes.getFloat
  def putFloat(value: Float) = bytes.putFloat(value)
}