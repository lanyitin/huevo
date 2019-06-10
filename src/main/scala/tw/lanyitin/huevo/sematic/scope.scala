package tw.lanyitin.huevo.sematic
import scala.util.{Try, Success, Failure}
import tw.lanyitin.huevo.lex._
import tw.lanyitin.common.ast.TokenType._
import tw.lanyitin.common.ast.{Token, NullToken, TokenType}
import tw.lanyitin.common.ast._

trait Scope {
  def find(identifier: Token): Try[ValueHolder]
  def put(identifier: Token, variable: ValueHolder): Scope
  def up: Scope
  def map: Map[String, ValueHolder]
  def next_local_id: Int = this.map.values.filter(_.isInstanceOf[Variable]).size
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