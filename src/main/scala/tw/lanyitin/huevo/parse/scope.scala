package tw.lanyitin.huevo.parse
import scala.util.{Try, Success, Failure}

trait ValueHolder {
  def typ: Type
}
case class Parameter(token: Token, typ: Type) extends ValueHolder
case class Variable(token: Token, typ: Type) extends ValueHolder
case class FunctionDeclaration(token: Token, parameters: List[Parameter], typ: Type) extends ValueHolder


trait Scope {
  def find(identifier: Token): Try[ValueHolder]
  def put(identifier: Token, variable: ValueHolder): Scope
  def up: Scope
  def map: Map[String, ValueHolder]
}

case class TopScope(map: Map[String, ValueHolder]=Map.empty) extends Scope {
  def find(identifier: Token): Try[ValueHolder] = {
    val r = map.get(identifier.txt)
    if (r.nonEmpty) {
      Success(r.get)
    } else {
      Failure(new Exception(s"unable to resolve type: ${identifier.txt}"))
    }
  }
  def put(identifier: Token, variable: ValueHolder) = TopScope(map + (identifier.txt -> variable))
  def up = null
}

case class SubScope(up: Scope, map: Map[String, ValueHolder]=Map.empty) extends Scope {
  def find(identifier: Token): Try[ValueHolder] = {
    val r = map.get(identifier.txt)
    if (r.nonEmpty) {
      Success(r.get)
    } else {
      up.find(identifier)
    }
  } 
  def put(identifier: Token, variable: ValueHolder) = SubScope(up, map + (identifier.txt -> variable))
}