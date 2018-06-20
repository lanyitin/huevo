package tw.lanyitin.huevo.machine
import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala .collection.immutable.Map
import java.io.PrintStream
import scala.util.Try
case class VM(
  _instructions: List[String],
  data_stack: Stack[HObject]=ListStack(Nil),
  frame_stack: Stack[Frame]=ListStack(Nil),
  ip: Int=0,
  stdout:PrintStream=System.out,
  stderr:PrintStream=System.err,
  debug: Boolean=false) {
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
      if (vm.ip >= vm.instructions.size) {
        vm
      } else {
        val instruction: List[String] = vm.instructions(vm.ip).split(" ").toList
        if (debug) {
          stdout.print(instruction + "\n")
        }
        val v2: Try[VM] = Try(instruction(0) match {
          case "nop" => vm.nop
          case "print" => vm.print
          case "push" => {
            val operand = instruction(1).toLowerCase
            if (operand == "true" || operand == "false") {
              vm.push(HObject(operand.toBoolean))
            } else if (operand.matches("\\d+(\\.\\d+)?")) {
              if (operand.contains(".")) {
                vm.push(HObject(operand.toFloat))
              } else {
                vm.push(HObject(operand.toInt))
              }
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
          case "boolean_equal" => vm.boolean_equal
          case "addi" => vm.addi
          case "addf" => vm.addf
          case "subi" => vm.subi
          case "subf" => vm.subf
          case "multiplyi" => vm.multiplyi
          case "multiplyf" => vm.multiplyf
          case "modi" => vm.modi
          case "modf" => vm.modf
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
          case "eqi" => vm.eqi
          case "eqf" => vm.eqf
          case "neqi" => vm.neqi
          case "neqf" => vm.neqf
          case "call" => vm.call(instruction(1).toInt)
          case "ret" => vm.ret
          case "load_param" => vm.load_param(instruction(1).toInt)
          case "halt" => vm
          case _ => throw new Exception(s"unexpected instruction: ${instruction}")
        })
        if (v2.isFailure) {
          stderr.println(v2.failed.get.getMessage)
          stderr.println(vm)
        }
        if (instruction(0) == "halt") {
          v2.get
        } else {
          loop(v2.get)
        }
      }
    }
    if (labels.contains("main")) {

      loop(this.copy(frame_stack=this.frame_stack.push(Frame(_instructions.size, Nil)), ip=labels("main")))
    } else {
      loop(this)
    }
  }
  def print = {
    stdout.println(data_stack.top.toString)
    this.copy(ip=this.ip+1)
  }
  def nop = {
    this.copy(ip=this.ip+1)
  }

  def jmp(addr: Int) = {
    this.copy(ip=addr)
  }

  def push(value: HObject) = {
    this.copy(data_stack=data_stack.push(value), ip=this.ip+1)
  }

  def pop = {
    val (v: HObject, s: Stack[HObject]) = data_stack.pop
    (v, this.copy(data_stack=s, ip=this.ip+1))
  }

  def duplicate = {
    this.copy(data_stack=data_stack.push(data_stack.top), ip=this.ip+1)
  }

  def swap = {
    val (a: HObject, s1: Stack[HObject]) = data_stack.pop
    val (b: HObject, s2: Stack[HObject]) = s1.pop
    this.copy(data_stack=data_stack.push(a).push(b), ip=this.ip+1)
  }

  def rotate = {
    val (a: HObject, s1: Stack[HObject]) = data_stack.pop
    val (b: HObject, s2: Stack[HObject]) = s1.pop
    val (c: HObject, s3: Stack[HObject]) = s2.pop
    this.copy(data_stack=data_stack.push(b).push(a).push(c), ip=this.ip+1)
  }

  def addi = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt + b.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def addf = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat + b.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def subi = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt - b.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def subf = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat - b.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def multiplyi = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt * b.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def multiplyf = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat * b.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def modi = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt % b.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)   
  }
  def modf = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat % b.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)   
  }
  def dividei = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(b.getInt / a.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def dividef = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(b.getFloat / a.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }

  def boolean_not = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val result = HObject(!a.getBoolean)
    this.copy(data_stack=s1.push(result), ip=this.ip+1)
  }

  def boolean_and = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getBoolean && b.getBoolean)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }

  def boolean_or = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getBoolean || b.getBoolean)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }

  def boolean_xor = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val (x:Boolean,y: Boolean) = (a.getBoolean, b.getBoolean)
    val result = HObject((x || y) && !(x && y))
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }

  def boolean_equal = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getBoolean == b.getBoolean)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }

  def jnz = {
    val (label:HObject, s1: Stack[HObject]) = data_stack.pop
    val (condition:HObject, s2: Stack[HObject]) = s1.pop
    if (condition.getBoolean) {
      this.copy(data_stack=s2, ip=label.getInt)
    } else {
      this.copy(data_stack=s2, ip=this.ip+1)
    }
  }

  def gti = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt > b.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def gtf = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat > b.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def gtei = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt >= b.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def gtef = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat >= b.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def lti = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt < b.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def ltf = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat < b.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def ltei = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt <= b.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  def ltef = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat <= b.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }

  def eqi = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt == b.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }

  def eqf = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat == b.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }

  def neqi = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getInt != b.getInt)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }

  def neqf = {
    val (a:HObject, s1: Stack[HObject]) = data_stack.pop
    val (b:HObject, s2: Stack[HObject]) = s1.pop
    val result = HObject(a.getFloat != b.getFloat)
    this.copy(data_stack=s2.push(result), ip=this.ip+1)
  }
  // TODO
  def load(hashCode: Integer) = ???

  def call(num_of_args: Int):VM = {
    val (function_address: HObject, s1: Stack[HObject]) = data_stack.pop
    val (arguments: List[HObject], s2: Stack[HObject]) = generate_argument_list(s1, num_of_args)
    val f = Frame(this.ip, arguments)
    this.copy(data_stack=s2, frame_stack=this.frame_stack.push(f), ip=function_address.getInt)
  }
  def ret = {
    val (f: Frame, s2:Stack[Frame]) = this.frame_stack.pop
    this.copy(ip=f.prev_ip + 1, frame_stack=s2)
  }

  def load_param(num: Int) = {
    this.copy(data_stack=this.data_stack.push(this.frame_stack.top.arguments(num)), ip=this.ip+1)
  }

  def generate_argument_list(stack: Stack[HObject], num: Int, acc: List[HObject]=Nil): (List[HObject], Stack[HObject]) = {
    if (num == 0) {
      (acc, stack)
    } else {
      val (obj: HObject, v2: Stack[HObject]) = stack.pop
      generate_argument_list(v2, num-1, obj :: acc)
    }
  }

  override def toString = {
    s"""VM(\n
      ${_instructions}
      ${ip}
      ${_instructions(ip)}
      ${data_stack}
    )"""
  }
}
