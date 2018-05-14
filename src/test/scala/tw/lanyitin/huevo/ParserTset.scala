package tw.lanyitin.huevo

import org.scalatest._
import java.io.File
import java.io.FileWriter

class ParserSpec extends FlatSpec with Matchers {
  import TokenType._
  // TODO: more test cases
  "A Parser" should "be able to parse function definition" in {
    val content = """
    |def add(a: Number, b: Number): Number = {
    |  a + b + (1 * 1 / 1) - 1
    |}
    |def addTwice(a: Number, b: Number): Number = {
    |  if (b > 0) {
    |    add(add(a, b), b)
    |  } else {
    |    a
    |  }
    |}
    """.stripMargin('|').trim()
    val scanner = Scanner(content)
    val (tree, state) = Parser.parse(scanner)
    // println(state.content.substring(state.state.position))
    // println(tree.gen_graphviz)
  }
  
}
