package tw.lanyitin.huevo.machine

import scala.collection.immutable.TreeMap

case class Heap(memory: TreeMap[Int, HObject]) {
  def find(key: Int) = memory(key)

  def put(obj: HObject) = Heap(memory.insert(obj.hashCode, obj))
}