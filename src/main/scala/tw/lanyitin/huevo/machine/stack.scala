package tw.lanyitin.huevo.machine


trait Stack[T] {
  def top: T
  def push(value: T): Stack[T]
  def pop: (T, Stack[T])
}

case class ListStack[T](list: List[T]) extends Stack[T] {
  def top = list.head
  def push(value: T) = ListStack(value :: list)
  def pop = (list.head, ListStack(list.tail))
}