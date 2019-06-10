package tw.lanyitin.huevo.parse
import java.util.Locale
import scala.annotation.tailrec
import tw.lanyitin.huevo.lex._
import tw.lanyitin.common.ast.TokenType._
import tw.lanyitin.common.ast.{Token, NullToken, TokenType}
import tw.lanyitin.huevo.sematic._
import tw.lanyitin.common.ast._

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
    } else if (expr.isInstanceOf[IdentifierExpression]) {
      this.visitIdentifierExpression(expr.asInstanceOf[IdentifierExpression])
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
  def visitIdentifierExpression(expr: IdentifierExpression): T
}
