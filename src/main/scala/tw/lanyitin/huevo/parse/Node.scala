package tw.lanyitin.huevo.parse
import java.util.Locale
import scala.util.{Try, Success, Failure}

trait Type {
  def toString: String
}
case class FunctionType(argType: List[Type], typ: Type) extends Type {
  override def toString = {
    "(%s): %s".format(argType.map(_.toString).mkString(", "), typ.toString)
  }
}
case object HBoolean extends Type {
  override def toString = "Boolean"
}
case object HInteger extends Type {
  override def toString = "Integer"
}
case object HFloat extends Type {
  override def toString = "Float"
}

trait ValueHolder {
  def typ: Type
}
case class Parameter(token: Token, typ: Type) extends ValueHolder
case class Variable(token: Token, typ: Type) extends ValueHolder
case class FunctionDeclaration(identifier: Token, parameters: List[Parameter], typ: Type) extends ValueHolder


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


trait TreeNode {
  def traverse(visitor: TreeVisitor)
  def visualize: String
}

trait Expression extends TreeNode {
  def id = this.hashCode.toString.replace("-", "")
  def typ: Type
}
abstract class DeclExpression() extends Expression

case class VariableDefinitionExpression(variable: Variable, value: Expression) extends DeclExpression() {
  def typ: Type = variable.typ
  def traverse(visitor: TreeVisitor) = {
    visitor.visit(value)
  }
  def visualize = {
    (this.value.visualize ::
    "%s -> %s".format(this.id, value.id) ::
    "%s [label=\"%s %d,%d\"]".format(this.id,
                                  this.variable.token.txt,
                                  this.variable.token.line,
                                  this.variable.token.col) :: Nil).mkString("\n")
  }
}

case class FunctionDefinitionExpression(declaration: FunctionDeclaration, body: Expression) extends DeclExpression() {
  def typ: Type = FunctionType(declaration.parameters.map(_.typ), declaration.typ)
  def traverse(visitor: TreeVisitor) = {
    visitor.visit(declaration)
    visitor.visit(body)
  }

  def visualize = {
    val arg_types = declaration.parameters.map(_.typ).map(_.toString).mkString(",")
    val ret_type = declaration.typ
    val identifier = declaration.identifier

    (body.visualize ::
    s"${this.id} -> ${body.id}\n" ::
    "%s [label=\"%s(%s):%s\"]".format(
      this.id,
      identifier.txt,
      arg_types,
      ret_type.toString
    ) :: Nil).mkString("\n")
  }
}

case class ExprsBlock(exprs: List[Expression]) extends Expression {
  def typ = exprs.last.typ

  def traverse(visitor: TreeVisitor) = {
    exprs.foreach(visitor.visit(_))
  }

  def visualize = {
    ("%s [label=\"\"]".format(this.id) :: 
    exprs.map(_.visualize) :::
    exprs.map(a => s"${this.id} -> ${a.id}")).mkString("\n")
  }
}

abstract class ValueExpression() extends Expression

case class IfExpression(condition: Expression, true_path: Expression, false_path: Expression = new ExprsBlock(Nil)) extends ValueExpression {
  def findCommonType(t1: Type, t2: Type): Type = t1
  def typ = {
    findCommonType(true_path.typ, false_path.typ)
  }
  def visualize = {
    val exprs = (condition :: true_path :: false_path :: Nil)
    ("%s -> %s [label=\"condition\"]".format(this.id, condition.id) ::
    "%s -> %s [label=\"true\"]".format(this.id, true_path.id) ::
    "%s -> %s [label=\"false\"]".format(this.id, false_path.id) ::
    "%s [label=\"if expression\"]".format(this.id) ::
    exprs.map(_.visualize)).mkString("\n")
  }

  def traverse(visitor: TreeVisitor) = {
    visitor.visit(condition)
    visitor.visit(true_path)
    visitor.visit(false_path)
  }
}

case class IdentifierExpression(token: Token, typ: Type) extends ValueExpression {
  def visualize =
    "%s [label=\"%s %d,%d\"]".format(this.id,
                                     this.token.txt,
                                     this.token.line,
                                     this.token.col)
  def traverse(visitor: TreeVisitor) = {
    visitor.visit(this)
  }
}

trait LiteralExpression[T] extends ValueExpression {
  def token: Token
  def value: T
  def visualize =
    "%s [label=\"%s %d,%d\"]".format(this.id,
                                     this.token.txt,
                                     this.token.line,
                                     this.token.col)
  def traverse(visitor: TreeVisitor) = {
    visitor.visit(this)
  }
}

case class BooleanLiteralExpression(token: Token, value: Boolean)
    extends LiteralExpression[Boolean] {
  def typ = HBoolean
}

case class FloatLiteralExpression(token: Token, value: Float) extends LiteralExpression[Float] {
  def typ = HFloat
}

case class IntegerLiteralExpression(token: Token, value: Int) extends LiteralExpression[Int] {
  def typ = HInteger
}

case class OperationCallExpression(token: Token, expr1: Expression, expr2: Expression) extends ValueExpression() {
  def findCommonType(expr1: Expression, expr2: Expression): Type = expr1.typ
  
  def typ = {
    token.txt match {
      case "=="|"!="|">="|"<="|">"|"<"|"and"|"or" => HBoolean
      case _ => findCommonType(expr1, expr2)
    }
  }
  def traverse(visitor: TreeVisitor) = {
    visitor.visit(expr1)
    visitor.visit(expr2)
    visitor.visit(this)
  }

  def visualize = {
    val exprs = (expr1 :: expr2 :: Nil)
    val viz: List[String] = exprs.map(_.visualize)
    val conn: List[String] = exprs.map(a => s"${this.id} -> ${a.id}")
    val label: String = "%s [label=\"%s %d,%d\"]".format(this.id,
                                     this.token.txt,
                                     this.token.line,
                                     this.token.col)
    (label :: viz ::: conn).mkString("\n")
  }
}

case class FunctionCallExpression(declaration: FunctionDeclaration, parameters: Expression*) extends ValueExpression {
  def typ: Type = declaration.typ
  def visualize = {
    ("%s [label=\"%s %d,%d\"]".format(this.id,
                                     declaration.identifier.txt,
                                     declaration.identifier.line,
                                     declaration.identifier.col) ::
    List(parameters:_*).map(_.visualize) :::
    List(parameters:_*).map(a => s"${this.id} -> ${a.id}")).mkString("\n")
  }

  def traverse(visitor: TreeVisitor) = {
    visitor.visit(this)
  }
}

abstract class TreeVisitor {
  def visit(expr: TreeNode) {
    if (expr.isInstanceOf[BooleanLiteralExpression]) {
      this.visitBooleanLiteral(expr.asInstanceOf[BooleanLiteralExpression])
    } else if (expr.isInstanceOf[IntegerLiteralExpression]) {
      this.visitIntegerLiteral(expr.asInstanceOf[IntegerLiteralExpression])
    } else if (expr.isInstanceOf[FloatLiteralExpression]) {
      this.visitFloatLiteral(expr.asInstanceOf[FloatLiteralExpression])
    } else if (expr.isInstanceOf[OperationCallExpression]) {
      this.visitOperationCallExp(expr.asInstanceOf[OperationCallExpression])
    } else {
      throw new Exception(s"unable to visit ${expr}")
    }
  }
  def visit(declaration: FunctionDeclaration)
  def visitBooleanLiteral(literal: BooleanLiteralExpression)
  def visitIntegerLiteral(literal: IntegerLiteralExpression)
  def visitFloatLiteral(literal: FloatLiteralExpression)
  def visitOperationCallExp(expr: OperationCallExpression)
}