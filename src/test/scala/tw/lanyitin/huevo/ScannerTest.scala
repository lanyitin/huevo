package tw.lanyitin.huevo

import org.scalatest._
import java.io.File
import java.io.FileWriter

class ScannerSpec extends FlatSpec with Matchers {

  "A Scanner" should "able to find tokens for function declaration" in {
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
    """.stripMargin('|')
    val scanner = Scanner(content, println)
    val tokens = takeWhile(scanner, (token) => token != EOFToken && token != UnexpectedToken)
      .filter(token => token match {
        case SpaceToken(_) => false
        case NewLineToken(_) => false
        case default => true
      })
    if (tokens.contains(UnexpectedToken)) {
      fail()
    } else {
      succeed
    }
  }

  def takeWhile(scanner: Scanner, p: (Token) => Boolean): List[Token] = {
    def loop(scanner: Scanner, acc: List[Token]): List[Token] = {
      val (token, next_state) = scanner.nextToken
      if (!p(token)) {
        token :: acc
      } else {
        loop(next_state, token :: acc)
      }
    }
    loop(scanner, Nil).reverse
  }
}
