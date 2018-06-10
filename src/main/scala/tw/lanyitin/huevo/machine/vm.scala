package tw.lanyitin.huevo.machine
import java.nio.ByteBuffer
import scala.annotation.tailrec
case class VM(instructions: List[String], stack: Stack[HObject]=ListStack(Nil), ip_stack: Stack[Int]=ListStack(0 :: Nil),  heap: Heap=Heap()) {

  def run = {
    @tailrec
    def loop(vm: VM):VM = {
      if (vm.ip_stack.top >= vm.instructions.size) {
        println(vm.stack.top)
        vm
      } else {
        val instruction: List[String] = vm.instructions(vm.ip_stack.top).split(" ").toList
        val v2: VM = if (instruction(0) == "nop") {
          vm.nop
        } else if (instruction(0) == "push") {
          val operand = instruction(1).toLowerCase
          if (operand == "true" || operand == "false") {
            vm.push(HObject(operand.toBoolean))
          } else {
            vm.push(HObject(operand.toInt))
          }
        } else if (instruction(0) == "pop") {
          vm.pop._2
        } else if (instruction(0) == "addi") {
          vm.addi
        } else if (instruction(0) == "addf") {
          vm.addf
        } else if (instruction(0) == "subi") {
          vm.subi
        } else if (instruction(0) == "subf") {
          vm.subf
        } else if (instruction(0) == "multiplyi") {
          vm.multiplyi
        } else if (instruction(0) == "multiplyf") {
          vm.multiplyf
        } else if (instruction(0) == "dividei") {
          vm.dividei
        } else if (instruction(0) == "dividef") {
          vm.dividef
        } else {
          throw new Exception(s"unexpected instruction: ${instruction}")
        }
        loop(v2)
      }
    }
    loop(this)
  }

  def nop = {
    this.copy(ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }

  def load(hashCode: Integer) = {
    this.copy(stack=stack.push(heap.find(hashCode)), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }

  def push(value: HObject) = {
    this.copy(stack=stack.push(value), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }

  def pop = {
    val (v: HObject, s: Stack[HObject]) = stack.pop
    (v, this.copy(stack=s, ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail)))
  }

  def duplicate = {
    this.copy(stack=stack.push(stack.top), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }

  def swap = {
    val (a: HObject, s1: Stack[HObject]) = stack.pop
    val (b: HObject, s2: Stack[HObject]) = s1.pop
    this.copy(stack=stack.push(a).push(b), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }

  def boolean_and = {
    val (a: HObject, s1: Stack[HObject]) = stack.pop
    val (b: HObject, s2: Stack[HObject]) = s1.pop
    val c = a.getBoolean && b.getBoolean
    this.copy(stack=stack.push(HObject(c)), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }

  def rotate = {
    val (a: HObject, s1: Stack[HObject]) = stack.pop
    val (b: HObject, s2: Stack[HObject]) = s1.pop
    val (c: HObject, s3: Stack[HObject]) = s2.pop
    this.copy(stack=stack.push(b).push(a).push(c), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }

  def addi = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt + b.getInt)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }
  def addf = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat + b.getFloat)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }
  def subi = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt - b.getInt)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }
  def subf = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat - b.getFloat)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }
  def multiplyi = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt * b.getInt)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }
  def multiplyf = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat * b.getFloat)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }
  def dividei = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(b.getInt / a.getInt)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }
  def dividef = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(b.getFloat / a.getFloat)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }

  def conditional_jump = {
    val (false_path:HObject, s1: Stack[HObject]) = stack.pop
    val (true_path:HObject, s2: Stack[HObject]) = s1.pop
    val (condition:HObject, s3: Stack[HObject]) = s2.pop
    if (condition.getBoolean) {
      this.copy(stack=s3, ip_stack=ListStack(true_path.getInt :: ip_stack.list.tail))
    } else {
      this.copy(stack=s3, ip_stack=ListStack(false_path.getInt :: ip_stack.list.tail))
    }
  }
}