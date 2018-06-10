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
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    println(instructions)
    val vm = VM(instructions).run
    assert(vm.stack.top.toString=="0.1")
  }
}
