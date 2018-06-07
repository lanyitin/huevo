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
    val result = Parser.parse(scanner)
    if (result.isFailure) {
      result.failed.get.printStackTrace
      fail(result.failed.get)
    } else {
     println(result.get._1.visualize)
    }
  }

  "A Parser" should "be able to parse comment starts with #" in {
    val content = """
    |#this is a comment
    |1 + 1 * 1 / 1
    """.stripMargin('|').trim()
    val scanner = Scanner(content)
    val result = Parser.parse(scanner)
    if (result.isFailure) {
      result.failed.get.printStackTrace
      fail(result.failed.get)
    } else {
     println(result.get._1.visualize)
    }
  }

  "A Parser" should "be able to parse if expression without else" in {
    val content = """
    |if (a > b) {1 + 1}
    """.stripMargin('|').trim()
    val scanner = Scanner(content)
    val result = Parser.parse(scanner)
    if (result.isFailure) {
      result.failed.get.printStackTrace
      fail(result.failed.get)
    } else {
     println(result.get._1.visualize)
    }
  }

  "A Parser" should "be able to parse if else" in {
    val content = """
    |if (a > b) {
    |  1 + 1
    |} else {
    |  2 + 1
    |}
    """.stripMargin('|').trim()
    val scanner = Scanner(content)
    val result = Parser.parse(scanner)
    if (result.isFailure) {
      result.failed.get.printStackTrace
      fail(result.failed.get)
    } else {
     println(result.get._1.visualize)
    }
  }

  "A Parser" should "be able to parse if else if" in {
    val content = """
    |if (a > b) {
    |  1 + 1
    |  2 + 2
    |} else if (a == b) {
    |  2 + 1
    |}
    """.stripMargin('|').trim()
    val scanner = Scanner(content)
    val result = Parser.parse(scanner)
    if (result.isFailure) {
      result.failed.get.printStackTrace
      fail(result.failed.get)
    } else {
     println(result.get._1.visualize)
    }
  }


}
