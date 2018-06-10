package tw.lanyitin.huevo.machine
import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala .collection.immutable.Map
import java.io.PrintStream
case class VM(_instructions: List[String], stack: Stack[HObject]=ListStack(Nil), ip_stack: Stack[Int]=ListStack(0 :: Nil),  heap: Heap=Heap(), stdout:PrintStream=System.out, stderr:PrintStream=System.err) {

  def label_analysis: (List[String], Map[String, Int]) = {
    @tailrec
    def loop(ip: Int, new_ip: Int, new_instructions: List[String], labels: Map[String, Int]): (List[String], Map[String, Int]) = {
      if (ip >= _instructions.size) {
        (new_instructions.reverse, labels)
      } else if (_instructions(ip).endsWith(":")) {
        loop(ip + 1, new_ip, new_instructions, labels + ((_instructions(ip).replace(":", ""), new_ip)))
      } else {
        loop(ip + 1, new_ip + 1, _instructions(ip) :: new_instructions, labels)
      }
    }
    loop(0, 0, Nil, Map.empty)
  }

  val (instructions: List[String], labels: Map[String, Int]) = label_analysis

  def run = {
    @tailrec
    def loop(vm: VM):VM = {
      if (vm.ip_stack.top >= vm.instructions.size) {
        vm
      } else {
        val instruction: List[String] = vm.instructions(vm.ip_stack.top).split(" ").toList
        val v2: VM = instruction(0) match {
          case "nop" => vm.nop
          case "print" => vm.print
          case "push" => {
            val operand = instruction(1).toLowerCase
            if (operand == "true" || operand == "false") {
              vm.push(HObject(operand.toBoolean))
            } else if (operand.matches("\\d+")) {
              vm.push(HObject(operand.toInt))
            } else {
              vm.push(HObject(labels(instruction(1))))
            }
          }
          case "pop" => vm.pop._2
          case "jmp" => {
            if (instruction(1).matches("\\d+")) {
              vm.jmp(instruction(1).toInt)
            } else {
              vm.jmp(labels(instruction(1)))
            }
          }
          case "jnz" => vm.jnz
          case "swap" => vm.swap
          case "rotate" => vm.rotate
          case "duplicate" => vm.duplicate
          case "boolean_and" => vm.boolean_and
          case "boolean_or" => vm.boolean_or
          case "boolean_not" => vm.boolean_not
          case "boolean_xor" => vm.boolean_xor
          case "addi" => vm.addi
          case "addf" => vm.addf
          case "subi" => vm.subi
          case "subf" => vm.subf
          case "multiplyi" => vm.multiplyi
          case "multiplyf" => vm.multiplyf
          case "dividei" => vm.dividei
          case "dividef" => vm.dividef
          case "gti" => vm.gti
          case "gtf" => vm.gtf
          case "gtei" => vm.gtei
          case "gtef" => vm.gtef
          case "lti" => vm.lti
          case "ltf" => vm.ltf
          case "ltei" => vm.ltei
          case "ltef" => vm.ltef
          case _ => throw new Exception(s"unexpected instruction: ${instruction}")
        }
        loop(v2)
      }
    }
    loop(this)
  }
  def print = {
    stdout.println(stack.top.toString)
    this.copy(ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }
  def nop = {
    this.copy(ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }

  def jmp(addr: Int) = {
    this.copy(ip_stack=ListStack(addr :: ip_stack.list.tail))
  }

  // TODO
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

  def boolean_not = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val result = HObject(!a.getBoolean)
    this.copy(stack=s1.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
  }

  def boolean_and = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getBoolean && b.getBoolean)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))    
  }

  def boolean_or = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getBoolean || b.getBoolean)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))    
  }

  def boolean_xor = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val (x:Boolean,y: Boolean) = (a.getBoolean, b.getBoolean)
    val result = HObject((x || y) && !(x && y))
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))    
  }

  def jnz = {
    val (label:HObject, s1: Stack[HObject]) = stack.pop
    val (condition:HObject, s2: Stack[HObject]) = s1.pop
    if (condition.getBoolean) {
      this.copy(stack=s2, ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))
    } else {
      this.copy(stack=s2, ip_stack=ListStack(label.getInt :: ip_stack.list.tail))
    }
  }

  def gti = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt > b.getInt)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))     
  }
  def gtf = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat > b.getFloat)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))     
  }
  def gtei = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt >= b.getInt)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))     
  }
  def gtef = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat >= b.getFloat)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))     
  }
  def lti = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt < b.getInt)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))     
  }
  def ltf = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat < b.getFloat)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))     
  }
  def ltei = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt <= b.getInt)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))     
  }
  def ltef = {
    val (a:HObject, s1: Stack[HObject]) = stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat <= b.getFloat)
    this.copy(stack=s2.push(result), ip_stack=ListStack((ip_stack.top + 1) :: ip_stack.list.tail))     
  }
}