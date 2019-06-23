package tw.lanyitin.huevo.machine

import org.scalatest._
import java.nio.ByteBuffer
import scala.collection.immutable.TreeMap

class VMSpec extends FlatSpec with Matchers {
  "A Virtual Machine" should "able to do arithmatic calculation" in {
    val instructions: List[String] = """
      |push 1
      |push 1
      |addi
      |push 10
      |multiplyi
      |push 50
      |dividei
      |push 1
      |addi
      |push 10
      |dividef
      |push false
      |boolean_and
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    val vm = VM(instructions,debug=false).run
    assert(vm.data_stack.top.toString=="false")
  }

  it should "able to handle if expression" in {
    val instructions: List[String] = """
      |push true
      |push false
      |boolean_or
      |push TRUE_PATH
      |jnz
      | push 1
      | push 1
      | addi
      | jmp END_OF_IF
      |TRUE_PATH:
      | push 2
      | push 2
      | multiplyi
      |END_OF_IF:
      | print 
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    val vm = VM(instructions,debug=false).run  
    assert(vm.data_stack.top.toString=="4")
  }

  it should "able to handle if expression (case 2)" in {
    val instructions: List[String] = """
      |push true
      |push false
      |boolean_and
      |push TRUE_PATH
      |jnz
      | push 1
      | push 1
      | addi
      | jmp END_OF_IF
      |TRUE_PATH:
      | push true
      | push true
      | boolean_xor
      |END_OF_IF:
      | print 
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    val vm = VM(instructions,debug=false).run    
    assert(vm.data_stack.top.toString=="2")
  }

  it should "able to handle function call" in {
    val instructions: List[String] = """
      |push 1
      |push 2
      |push add_int
      |call 2
      |print
      |duplicate
      |push add_int
      |call 2
      |halt
      |add_int:
      |load_param 0
      |load_param 1
      |addi
      |ret
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    val vm = VM(instructions,debug=false).run
    assert(vm.data_stack.top.toString=="6")
  }
}