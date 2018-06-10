package tw.lanyitin.huevo.parse
import java.util.Locale

trait Type {
  def toString: String
}
case class NamedType(token: Token) extends Type {
  override def toString = token.txt
}
case class LambdaType(argType: List[Type], retType: Type) extends Type {
  override def toString = {
    "(%s): %s".format(argType.map(_.toString).mkString(", "), retType.toString)
  }
}
case class Parameter(token: Token, parameterType: Type)
case class FunctionDeclaration(identifier: Token, parameters: List[Parameter], ret_type: Type)


trait TreeNode {
  def traverse(visitor: TreeVisitor)
  def visualize: String
}

trait Expression extends TreeNode {
  def id = this.hashCode.toString.replace("-", "")
}
abstract class DeclExpression() extends Expression


case class FunctionDefinitionExpression(declaration: FunctionDeclaration, body: Expression) extends DeclExpression() {
  def traverse(visitor: TreeVisitor) = {
    visitor.visit(declaration)
    visitor.visit(body)
  }

  def visualize = {
    val arg_types = declaration.parameters.map(_.parameterType).map(_.toString).mkString(",")
    val ret_type = declaration.ret_type
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

case class IfExpression(condition: Expression, true_path: Expression, fales_path: Expression = new ExprsBlock(Nil)) extends ValueExpression {
  def visualize = {
    val exprs = (condition :: true_path :: fales_path :: Nil)
    ("%s -> %s [label=\"condition\"]".format(this.id, condition.id) ::
    "%s -> %s [label=\"true\"]".format(this.id, true_path.id) ::
    "%s -> %s [label=\"false\"]".format(this.id, fales_path.id) ::
    "%s [label=\"if expression\"]".format(this.id) ::
    exprs.map(_.visualize)).mkString("\n")
  }

  def traverse(visitor: TreeVisitor) = {
    visitor.visit(condition)
    visitor.visit(true_path)
    visitor.visit(fales_path)
  }
}

case class IdentifierExpression(token: Token) extends ValueExpression {
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
    extends LiteralExpression[Boolean]

case class NumberLiteralExpression(token: Token, value: Number) extends LiteralExpression[Number]

case class OperationCallExpression(token: Token, expr1: Expression, expr2: Expression) extends ValueExpression() {
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

case class FunctionCallExpression(identifier: Token, parameters: Expression*) extends ValueExpression {
  def visualize = {
    ("%s [label=\"%s %d,%d\"]".format(this.id,
                                     this.identifier.txt,
                                     this.identifier.line,
                                     this.identifier.col) ::
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
      this.visitBooleanExpr(expr.asInstanceOf[BooleanLiteralExpression])
    } else if (expr.isInstanceOf[NumberLiteralExpression]) {
      this.visitNumberExpr(expr.asInstanceOf[NumberLiteralExpression])
    } else if (expr.isInstanceOf[OperationCallExpression]) {
      this.visitOperationCallExp(expr.asInstanceOf[OperationCallExpression])
    } else {
      throw new Exception(s"unable to visit ${expr}")
    }
  }
  def visit(declaration: FunctionDeclaration)
  def visitBooleanExpr(literal: BooleanLiteralExpression)
  def visitNumberExpr(literal: NumberLiteralExpression)
  def visitOperationCallExp(expr: OperationCallExpression)
}