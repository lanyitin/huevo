package tw.lanyitin.huevo

import parse._
import machine.VM

object Main {
  var vm: VM = VM(Nil,debug=true)
  def top = vm.stack.top
  def main(args: Array[String]): Unit = {
    var running: Boolean = true
    var inputString: String = ""
    while (running) {
      print(">> ")
      Console.out.flush
      inputString = scala.io.StdIn.readLine()
      if (inputString == "exit") {
        running = false
      } else {
        eval(inputString)
        println(this.top)
      }
    }
  }

  def eval(str: String): Unit = {
    val scanner = Scanner(str)
    val result = Parser.parse(scanner)
    if (result.isFailure) {
      System.err.println(result.get)
    } else {
      val inst:List[String] = compile(result.get._1)
      vm=vm.copy(_instructions=vm.instructions ::: inst).run
    }
  }

  def compiler: TreeVisitor[List[String]] = new TreeVisitor[List[String]] {
    def flat: List[List[String]] => List[String] = (a: List[List[String]]) => a.flatten
    def operatorMap: Map[String, String] = Map("+" -> "add", "-" -> "sub", "*" -> "multiply", "/" -> "divide")
    def visitBooleanLiteral(literal: BooleanLiteralExpression): List[String] = List(s"push ${literal.value}")
    def visitFloatLiteral(literal: FloatLiteralExpression): List[String] = List(s"push ${literal.value}")
    def visitIntegerLiteral(literal: IntegerLiteralExpression): List[String] = List(s"push ${literal.value}")
    def visitOperationCallExp(expr: OperationCallExpression): List[String] = {
      this.visit(expr.expr1) ::: this.visit(expr.expr2) ::: ({
        if (expr.typ == HInteger) {
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
  }

  def compile(ast: Expression): List[String] = compiler.visit(ast)
}