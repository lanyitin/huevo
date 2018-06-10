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
      |print
      |push 10
      |multiplyi
      |print
      |push 50
      |dividei
      |print
      |push 1
      |addi
      |print
      |push 10
      |dividef
      |print
      |push false
      |boolean_and
    """.stripMargin('|').split("\\n").toList.map(_.trim).filter(_.length > 0)
    val vm = VM(instructions).run
    assert(vm.stack.top.toString=="false")
  }
}
