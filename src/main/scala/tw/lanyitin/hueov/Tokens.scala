package tw.lanyitin.huevo

object TokenType extends Enumeration {
  type TokenType = Value
val SpaceToken = Value("SpaceToken") 
val NewLineToken = Value("NewLineToken") 
val EOFToken = Value("EOFToken") 
val UnexpectedToken = Value("UnexpectedToken") 
val NumberToken = Value("NumberToken") 
val StringToken = Value("StringToken") 
val LParanToken = Value("LParanToken") 
val RParanToken = Value("RParanToken") 
val LCurlyBracket = Value("LCurlyBracket") 
val RCurlyBracket = Value("RCurlyBracket") 
val CommaToken = Value("CommaToken") 
val ColumnToken = Value("ColumnToken") 
val DefToken = Value("DefToken") 
val IfToken = Value("IfToken") 
val ElseToken = Value("ElseToken") 
val IdentifierToken = Value("IdentifierToken") 
val OperatorToken = Value("OperatorToken")
val CommentHeadToken = Value("CommentHeadToken")
val CommentBodyToken = Value("CommentBodyToken")
val QuoteToken = Value("QuoteToken")
// TODO: is it possible to eliminate NotExistToken?
val NotExistToken = Value("")
}

object Token {
  var tokenId: Integer = 0
  def apply(tokenType: TokenType.TokenType, txt: String, line: Integer = 0, col: Integer = 0): Token = {
    new Token(tokenType, txt, line, col)
  }
}

sealed class Token(val tokenType: TokenType.TokenType, val txt: String, val line: Integer = 0, val col: Integer = 0) {
  val id = Token.tokenId
  Token.tokenId = Token.tokenId + 1

  override def toString: String = {
    val espedTxt = txt.replace("\n","\\n").replace("\r", "\\r")
    s"${tokenType}('${espedTxt}')"
  }
}

object MatcherGenerator {
  import TokenType._
  type TokenMatcher = Scanner => (Boolean, Scanner, List[Token])
  // TODO: making matchers printable

  // TODO: implement following helper matchers
  def zeroOrMore(matcher: TokenMatcher): TokenMatcher = ???
  def oneOrMore(matcher: TokenMatcher): TokenMatcher = ???
  def byType(tokenType: TokenType): TokenMatcher = {
    (scanner) => {
      val (token, next) = scanner.nextToken
      if (token.tokenType == tokenType) {
        (true, next, List(token))
      } else {
        (false, scanner, Nil)
      }
    }
  }

  def byText(txt: String): TokenMatcher = {
    (scanner) => {
      val (token, next) = scanner.nextToken
      if (token.txt == txt) {
        (true, next, List(token))
      } else {
        (false, scanner, Nil)
      }
    }
  }

  def and(m1: TokenMatcher, m2: TokenMatcher): TokenMatcher = {
    (scanner) => {
      val (r1, next, token) = m1(scanner)
      val (r2, _, _) = m2(scanner)
      if (r1 && r2) {
        (true, next, token)
      } else {
        (false, scanner, Nil)
      }
    }
  }

  def or(m1: TokenMatcher, m2: TokenMatcher): TokenMatcher = {
    (scanner) => {
      val (r1, next, token) = m1(scanner)
      if (r1) {
        (true, next, token)
      } else {
        val (r2, next2, token2) = m2(scanner)
        if (r2) {
          (true, next2, token2)
        } else {
          (false, scanner, Nil)
        }
      }
    }
  }

  def epsilon: TokenMatcher = {
    (scanner) => (true, scanner, Nil)
  }
}