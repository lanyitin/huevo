package tw.lanyitin.huevo.parse

import org.scalatest._
import java.io.File
import java.io.FileWriter
import tw.lanyitin.huevo.lex.Scanner

class ParserSpec extends FlatSpec with Matchers {
  // TODO: more test cases
  "A Parser" should "be able to parse function definition" in {
    val content = """
    |def add(a: Float, b: Float): Float = {
    |  a + b + (1 * 1 / 1) - 1
    |}
    |def addTwice(a: Float, b: Float): Float = {
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
    //  println(result.get._1.visualize)
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
    //  println(result.get._1.visualize)
    }
  }

  "A Parser" should "be able to parse if expression without else" in {
    val content = """
    |def validate(a:Integer, b: Integer): Boolean = if (a > b) {true}
    """.stripMargin('|').trim()
    val scanner = Scanner(content)
    val result = Parser.parse(scanner)
    if (result.isFailure) {
      result.failed.get.printStackTrace
      fail(result.failed.get)
    } else {
    //  println(result.get._1.visualize)
    }
  }

  "A Parser" should "be able to parse if else" in {
    val content = """
    |def test2(a: Integer, b: Integer): Integer =
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
    //  println(result.get._1.visualize)
    }
  }

  "A Parser" should "be able to parse if else if" in {
    val content = """
    |let a: Integer = 2
    |let b: Integer = 1
    |if (a > b) {
    |  1 + 1
    |  2 + 2
    |} else if (a == b) {
    |  2 + 1
    |} else {
    |  4
    |}
    """.stripMargin('|').trim()
    val scanner = Scanner(content)
    val result = Parser.parse(scanner)
    if (result.isFailure) {
      result.failed.get.printStackTrace
      fail(result.failed.get)
    } else {
    //  println(result.get._1.visualize)
    }
  }
}
