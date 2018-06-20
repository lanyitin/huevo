package tw.lanyitin.huevo.sematic

trait Type {
  def toString: String
  def parent: Type
}

case object AnyValue extends Type {
  override                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           def toString = "Unit"
  def parent = this
}
case class FunctionType(argType: List[Type], typ: Type) extends Type {
  override def toString = {
    (typ :: argType.reverse).reverse.mkString(" => ")
  }
  def parent = AnyValue
}
case object HBoolean extends Type {
  override def toString = "Boolean"
  def parent = AnyValue
}
case object HNumber extends Type {
  override def toString = "Number"
  def parent = AnyValue
}
case object HInteger extends Type {
  override def toString = "Integer"
  def parent = HNumber
}
case object HFloat extends Type {
  override def toString = "Float"
  def parent = HNumber
}