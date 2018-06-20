package tw.lanyitin.huevo

import parse._
import lex._
import machine.VM
// import scala.scalajs.js.annotation.JSExport

// @JSExport
object Main {
  var vm: VM = VM(Nil,debug=false)
  def top = vm.data_stack.top

  // @JSExport
  def eval(str: String, print_byte_code: Boolean=false): Unit = {
    val scanner = Scanner(str)
    val result = Parser.parse(scanner)
    if (result.isFailure) {
      System.err.println(result.get)
    } else {
      val inst:List[String] = compile(result.get._1)
      if (print_byte_code) {
        println(inst)
      }
      vm=VM(inst,debug=false).run
    }
  }

  var compiler = Compiler()

  def compile(ast: Expression): List[String] = compiler.visit(ast)
}