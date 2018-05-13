package tw.lanyitin.huevo

import org.scalatest._
import java.io.File
import java.io.FileWriter

class ParserSpec extends FlatSpec with Matchers {
  import TokenType._

  "A Parser" should "able to parse arithmatic expression" in {
    val content = """
    | 1 + 2 * 3 / 4 * (1 + 2) / (2 * 4) + (( 1 + 2) * 3)
    """.stripMargin('|').trim()
    val scanner = Scanner(content, println)
    Parser.parse(scanner)

    val content2 = "a + c + (1 * 1 / 1) - 1"
    println(Parser.parse(Scanner(content2, println))._1)
  }
  it should "be able to parse function definition" in {
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
    val scanner = Scanner(content, println)
    val (tree, _) = Parser.parse(scanner)
    println(Parser.gen_graphviz(tree))
  }
  
}
