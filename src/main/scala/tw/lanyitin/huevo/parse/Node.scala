package tw.lanyitin.huevo.parse
import java.util.Locale
import scala.annotation.tailrec

trait TreeNode {
  def visualize: String
}

trait Expression extends TreeNode {
  def id = this.hashCode.toString.replace("-", "")
  def typ: Type
}
abstract class DeclExpression() extends Expression

case class VariableDefinitionExpression(variable: Variable, value: Expression)
    extends DeclExpression() {
  def typ: Type = variable.typ

  def visualize = {
    (this.value.visualize ::
      "%s -> %s".format(this.id, value.id) ::
      "%s [label=\"%s:%s %d,%d\"]".format(this.id,
                                       this.variable.token.txt,
                                       this.typ,
                                       this.variable.token.line,
                                       this.variable.token.col) :: Nil)
      .mkString("\n")
  }
}

case class FunctionDefinitionExpression(declaration: FunctionDeclaration,
                                        body: Expression)
    extends DeclExpression() {
  def typ: Type =
    FunctionType(declaration.parameters.map(_.typ), declaration.typ)

  def visualize = {
    val arg_types =
      declaration.parameters.map(_.typ).map(_.toString).mkString(",")
    val ret_type = declaration.typ
    val identifier = declaration.token

    (body.visualize ::
      s"${this.id} -> ${body.id}\n" ::
      "%s [label=\"%s: %s\"]".format(
      this.id,
      identifier.txt,
      typ.toString
    ) :: Nil).mkString("\n")
  }
}

case class ExprsBlock(exprs: List[Expression]) extends Expression {
  def typ = if (exprs.size > 0) exprs.last.typ else AnyValue

  def visualize = {
    ("%s [label=\"%s\"]".format(this.id, this.typ) ::
      exprs.map(_.visualize) :::
      exprs.map(a => s"${this.id} -> ${a.id}")).mkString("\n")
  }
}

abstract class ValueExpression() extends Expression

case class IfExpression(condition: Expression,
                        true_path: Expression,
                        false_path: Expression = new ExprsBlock(Nil))
    extends ValueExpression {
  def findCommonType(t1: Type, t2: Type): Type = {
    @tailrec
    def loop(t: Type, acc: List[Type]): List[Type] = {
      if (t.parent == t) {
        t :: acc
      } else {
        loop(t.parent, t :: acc)
      }
    }
    loop(t1, Nil).intersect(loop(t2, Nil)).last
  }
  def typ = {
    findCommonType(true_path.typ, false_path.typ)
  }
  def visualize = {
    val exprs = (condition :: true_path :: false_path :: Nil)
    ("%s -> %s [label=\"condition\"]".format(this.id, condition.id) ::
      "%s -> %s [label=\"true\"]".format(this.id, true_path.id) ::
      "%s -> %s [label=\"false\"]".format(this.id, false_path.id) ::
      "%s [label=\"if expression: %s\"]".format(this.id, this.typ.toString) ::
      exprs.map(_.visualize)).mkString("\n")
  }
}

case class IdentifierExpression(token: Token, typ: Type)
    extends ValueExpression {
  def visualize =
    "%s [label=\"%s:%s %d,%d\"]".format(this.id,
                                     this.token.txt,
                                     this.typ,
                                     this.token.line,
                                     this.token.col)
}

trait LiteralExpression[T] extends ValueExpression {
  def token: Token
  def value: T
  def visualize =
    "%s [label=\"%s:%s %d,%d\"]".format(this.id,
                                     this.token.txt,
                                     this.typ,
                                     this.token.line,
                                     this.token.col)
}

case class BooleanLiteralExpression(token: Token, value: Boolean)
    extends LiteralExpression[Boolean] {
  def typ = HBoolean
}

case class FloatLiteralExpression(token: Token, value: Float)
    extends LiteralExpression[Float] {
  def typ = HFloat
}

case class IntegerLiteralExpression(token: Token, value: Int)
    extends LiteralExpression[Int] {
  def typ = HInteger
}

case class OperationCallExpression(token: Token,
                                   expr1: Expression,
                                   expr2: Expression)
    extends ValueExpression() {
  def findCommonType(expr1: Expression, expr2: Expression): Type = {
    @tailrec
    def loop(t: Type, acc: List[Type]): List[Type] = {
      if (t.parent == t) {
        t :: acc
      } else {
        loop(t.parent, t :: acc)
      }
    }
    loop(expr1.typ, Nil).intersect(loop(expr2.typ, Nil)).last
  }
  def typ = {
    token.txt match {
      case "==" | "!=" | ">=" | "<=" | ">" | "<" | "and" | "or" => HBoolean
      case _                                                    => findCommonType(expr1, expr2)
    }
  }

  def visualize = {
    val exprs = (expr1 :: expr2 :: Nil)
    val viz: List[String] = exprs.map(_.visualize)
    val conn: List[String] = exprs.map(a => s"${this.id} -> ${a.id}")
    val label: String = "%s [label=\"%s:%s %d,%d\"]".format(this.id,
                                                         this.token.txt,
                                                         this.typ,
                                                         this.token.line,
                                                         this.token.col)
    (label :: viz ::: conn).mkString("\n")
  }
}

case class FunctionCallExpression(declaration: FunctionDeclaration,
                                  parameters: Expression*)
    extends ValueExpression {
  def typ: Type = declaration.typ
  def visualize = {
    ("%s [label=\"%s %d,%d\"]".format(this.id,
                                      declaration.token.txt,
                                      declaration.token.line,
                                      declaration.token.col) ::
      List(parameters: _*).map(_.visualize) :::
      List(parameters: _*).map(a => s"${this.id} -> ${a.id}")).mkString("\n")
  }
}

abstract class TreeVisitor[T] {
  def flat: List[T] => T
  def visit(expr: TreeNode): T = {
    if (expr.isInstanceOf[BooleanLiteralExpression]) {
      this.visitBooleanLiteral(expr.asInstanceOf[BooleanLiteralExpression])
    } else if (expr.isInstanceOf[IntegerLiteralExpression]) {
      this.visitIntegerLiteral(expr.asInstanceOf[IntegerLiteralExpression])
    } else if (expr.isInstanceOf[FloatLiteralExpression]) {
      this.visitFloatLiteral(expr.asInstanceOf[FloatLiteralExpression])
    } else if (expr.isInstanceOf[OperationCallExpression]) {
      this.visitOperationCallExp(expr.asInstanceOf[OperationCallExpression])
    } else if (expr.isInstanceOf[ExprsBlock]) {
      val e = expr.asInstanceOf[ExprsBlock]
      flat(e.exprs.map(e2 => this.visit(e2)))
    } else if (expr.isInstanceOf[IfExpression]) {
      this.visitIfExpression(expr.asInstanceOf[IfExpression])
    } else if (expr.isInstanceOf[FunctionCallExpression]) {
      this.visitFunctionCallExpression(expr.asInstanceOf[FunctionCallExpression])
    } else if (expr.isInstanceOf[FunctionDefinitionExpression]) {
      this.visitFunctionDefinitionExpression(expr.asInstanceOf[FunctionDefinitionExpression])
    } else {
      throw new Exception(s"unable to visit ${expr}")
    }
  }
  def visitBooleanLiteral(literal: BooleanLiteralExpression): T
  def visitIntegerLiteral(literal: IntegerLiteralExpression): T
  def visitFloatLiteral(literal: FloatLiteralExpression): T
  def visitOperationCallExp(expr: OperationCallExpression): T
  def visitIfExpression(expr: IfExpression): T
  def visitFunctionCallExpression(exrp: FunctionCallExpression): T
  def visitFunctionDefinitionExpression(exrp: FunctionDefinitionExpression): T
}
