package tw.lanyitin.huevo.machine

import java.nio.ByteBuffer

object HObject {
  def intToByteArray(a: Int): Array[Byte] = {
    ByteBuffer.allocate(4).putInt(a).array()
  }

  def floatToByteArray(a: Float): Array[Byte] = {
    ByteBuffer.allocate(4).putFloat(a).array()
  }

  def apply(i: Int): HInteger = {
    HInteger(intToByteArray(i))
  }

  def apply(f: Float): HFloat = {
    HFloat(floatToByteArray(f))
  }

  def apply(b: Boolean): HObject = {
    if (b) {
      HBoolean(Array[Byte](0xFF.toByte))
    } else {
      HBoolean(Array[Byte](0x00.toByte))
    }
  }
}

trait HObject {
  override def toString: String
  def getInt: Int
  def getFloat: Float
  def getBoolean: Boolean 
}


case class HInteger(bytes: Array[Byte]) extends HObject {
  override def toString = ByteBuffer.wrap(bytes).getInt.toString
  def getInt = ByteBuffer.wrap(bytes).getInt
  def getFloat = ByteBuffer.wrap(bytes).getInt.toFloat
  def getBoolean = getInt != 0x00000000
}

case class HFloat(bytes: Array[Byte]) extends HObject {
  override def toString = ByteBuffer.wrap(bytes).getFloat.toString
  def getInt = ByteBuffer.wrap(bytes).getFloat.toInt
  def getFloat = ByteBuffer.wrap(bytes).getFloat
  def getBoolean = getFloat != 0x00000000 
}

case class HBoolean(bytes: Array[Byte]) extends HObject {
  override def toString = {
    if (this.getBoolean) {
      "true"
    } else {
      "false"
    }
  }
  def getInt = throw new Exception("cant getInt from HBoolean")
  def getFloat = throw new Exception("cant getFloat from HBoolean")
  def getBoolean = bytes(0) != 0 
}