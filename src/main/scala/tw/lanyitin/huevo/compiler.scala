package tw.lanyitin.huevo

import tw.lanyitin.huevo.sematic._
import tw.lanyitin.huevo.parse._
import scala.util.Random

case class Compiler() extends TreeVisitor[List[String]] {
    val random = new Random()
    def flat: List[List[String]] => List[String] = (a: List[List[String]]) => a.flatten
    def operatorMap: Map[String, String] = Map(
      "+" -> "add",
      "-" -> "sub",
      "*" -> "multiply",
      "/" -> "divide",
      ">" -> "gt",
      "<" -> "lt",
      ">=" -> "gte",
      "<=" -> "lte",
      "==" -> "eq",
      "!=" -> "neq",
      "%" -> "mod"
    )
    def visitBooleanLiteral(literal: BooleanLiteralExpression): List[String] = List(s"push ${literal.value}")
    def visitFloatLiteral(literal: FloatLiteralExpression): List[String] = List(s"push ${literal.value}")
    def visitIntegerLiteral(literal: IntegerLiteralExpression): List[String] = List(s"push ${literal.value}")
    def visitOperationCallExp(expr: OperationCallExpression): List[String] = {
      this.visit(expr.expr1) ::: this.visit(expr.expr2) ::: ({
        if (expr.typ == HInteger || (expr.typ == HBoolean && expr.expr1.typ == HInteger && expr.expr2.typ == HInteger)) {
          s"${operatorMap(expr.token.txt)}i"
        } else if (expr.typ == HFloat || expr.typ == HNumber) {
          s"${operatorMap(expr.token.txt)}f"
        } else if (expr.typ == HBoolean) {
          s"boolean_${expr.token.txt}"
        } else {
          expr.token.txt
        }
      } :: Nil)
    }
    def visitIfExpression(expr: IfExpression): List[String] = {
      var id = random.nextInt
      while (id < 0) {
        id = random.nextInt
      }
      this.visit(expr.condition) ::: 
       List(s"push TRUE_OF_IF_${id}", s"jnz") ::: 
       this.visit(expr.false_path) :::
       List(s"jmp END_OF_IF_${id}") ::: 
       List(s"TRUE_OF_IF_${id}:") ::: 
       this.visit(expr.true_path) ::: 
       List(s"END_OF_IF_${id}:")
    }
    def visitFunctionCallExpression(expr: FunctionCallExpression): List[String] = {
      val parameter_codes:List[String] = expr.parameters.map(this.visit(_)).toList.flatten
      val code: List[String] = List(parameter_codes, List(s"push ${expr.declaration.token.txt}",s"call ${expr.declaration.parameters.size}")).flatten
      code
    }
    def visitFunctionDefinitionExpression(expr: FunctionDefinitionExpression): List[String] = {
      (s"${expr.declaration.token.txt}:" :: this.visit(expr.body)) ++ List("ret")
    }
    def visitIdentifierExpression(expr: IdentifierExpression): List[String] = {
      expr.holder match {
        case Parameter(token, typ, idx) => List(s"load_param ${idx}")
        case Variable(_, _, _) => throw new Exception(s"unable to handle local variable ${expr}")
        case FunctionDeclaration(_, _, _) => throw new Exception("unable to handle local FunctionDeclaration")
      }
    }
  }