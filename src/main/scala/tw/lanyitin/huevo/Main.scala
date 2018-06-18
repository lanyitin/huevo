package tw.lanyitin.huevo

import parse._
import machine.VM
import scala.util.Random

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
      "!=" -> "neq"
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
      val ret = this.visit(expr.condition) ::: 
       List(s"push TRUE_OF_IF_${id}", s"jnz") ::: 
       this.visit(expr.false_path) :::
       List(s"jmp END_OF_IF_${id}") ::: 
       List(s"TRUE_OF_IF_${id}:") ::: 
       this.visit(expr.true_path) ::: 
       List(s"END_OF_IF_${id}:")
      println("===\n" + ret.mkString("\n") + "====\n")
      ret
    }
  }

  def compile(ast: Expression): List[String] = compiler.visit(ast)
}