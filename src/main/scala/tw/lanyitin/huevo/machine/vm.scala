package tw.lanyitin.huevo.machine
import java.nio.ByteBuffer
case class VM(stack: Stack[HObject], instructions: ByteBuffer, program_counter: Int, heap: Heap) {

  def load(hashCode: Integer) = {
    this.copy(stack.push(heap.find(hashCode)))
  }

  def push(value: HObject) = {
    this.copy(stack=stack.push(value))
  }

  def pop = {
    val (v: HObject, s: Stack[HObject]) = stack.pop
    (v, this.copy(stack=s))
  }

  def duplicate = {
    this.copy(stack=stack.push(stack.top))
  }

  def rotate = {
    val (a: HObject, s1: Stack[HObject]) = stack.pop
    val (b: HObject, s2: Stack[HObject]) = s1.pop
    val (c: HObject, s3: Stack[HObject]) = s2.pop
    this.copy(stack.push(b).push(a).push(c))
  }

  def addi = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HNumber(a.asInstanceOf[HNumber].getInt + b.asInstanceOf[HNumber].getInt)
    this.copy(stack=s2.push(result))
  }
  def subi = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HNumber(a.asInstanceOf[HNumber].getInt - b.asInstanceOf[HNumber].getInt)
    this.copy(stack=s2.push(result))
  }
  def multiplyi = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HNumber(a.asInstanceOf[HNumber].getInt * b.asInstanceOf[HNumber].getInt)
    this.copy(stack=s2.push(result))
  }
  def dividei = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HNumber(b.asInstanceOf[HNumber].getInt / a.asInstanceOf[HNumber].getInt)
    this.copy(stack=s2.push(result))
  }
}