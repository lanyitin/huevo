package tw.lanyitin.huevo

import org.scalatest._

class ScannerSpec extends FlatSpec with Matchers {

  "A Scanner" should "return a keywrod toekn for 'def'" in {
    val scanner = new Scanner("def")
    val token = scanner.NextToken
    token.Text should be ("def")
    token.Type should be (TokenType.Keyword)
    token.LineNum should be (1)
    token.StartCol should be (0)
    token.EndCol should be (2)
  }

  it should "return a identifier toekn for 'fibonacci'" in {
    val scanner = new Scanner("fibonacci")
    val token = scanner.NextToken
    token.Text should be ("fibonacci")
    token.Type should be (TokenType.Identifier)
    token.LineNum should be (1)
    token.StartCol should be (0)
    token.EndCol should be (8)
  }

  it should "return a number toekn for '1'" in {
    val scanner = new Scanner("1")
    val token = scanner.NextToken
    token.Text should be ("1")
    token.Type should be (TokenType.Number)
    token.LineNum should be (1)
    token.StartCol should be (0)
    token.EndCol should be (0)
  }

  it should "return a keywrod toekn for 'def' and than identifier token for 'fibonacci'" in {
    val scanner = new Scanner("def fibonacci")
    val token1 = scanner.NextToken
    token1.Text should be ("def")
    token1.Type should be (TokenType.Keyword)
    token1.LineNum should be (1)
    token1.StartCol should be (0)
    token1.EndCol should be (2)

    val token2 = scanner.NextToken
    token2.Text should be ("fibonacci")
    token2.Type should be (TokenType.Identifier)
    token2.LineNum should be (1)
    token2.StartCol should be (4)
    token2.EndCol should be (12)
  }

}
