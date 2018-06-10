package tw.lanyitin.huevo.machine

import org.scalatest._
import java.nio.ByteBuffer
import scala.collection.immutable.TreeMap

class VMSpec extends FlatSpec with Matchers {
  "A Virtual Machine" should "able to do arithmatic calculation" in {
    val vm = VM(ListStack(Nil), ByteBuffer.allocate(0), 0, Heap(new TreeMap()))
    val vm2 = vm.push(HNumber(1)).push(HNumber(1)).addi
    assert(vm2.stack.top.toString == "2")
    val vm3 = vm2.push(HNumber(10)).multiplyi
    assert(vm3.stack.top.toString == "20")
    val vm4 = vm3.push(HNumber(4)).dividei
    assert(vm4.stack.top.toString == "5")
  }
}
