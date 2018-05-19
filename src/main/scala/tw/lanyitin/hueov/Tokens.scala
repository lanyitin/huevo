package tw.lanyitin.huevo

import scala.util.{Try, Success, Failure}

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
val BooleanConstantToken = Value("BooleanConstantToken")
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
  trait TokenMatcher {
    def apply(scanner: Scanner): Try[(List[Token], Scanner)]

    def and (m2: TokenMatcher): TokenMatcher = {
      val self = this
      new TokenMatcher {
        def apply(scanner: Scanner): Try[(List[Token], Scanner)] = {
          val r1 = self(scanner)
          if (r1.isFailure) {
            r1
          } else {
            m2(scanner)
          }
        }

        override def toString: String = "[" + self.toString + "&" + m2.toString + "]"
      }
    }

    def or (m2: TokenMatcher): TokenMatcher = {
      val self = this
      new TokenMatcher {
        def apply(scanner: Scanner): Try[(List[Token], Scanner)] = {
          val r1 = self(scanner)
          if (r1.isSuccess) {
            r1
          } else {
            m2(scanner)
          }
        }

        override def toString: String = s"(${self.toString} | ${m2.toString})"
      }
    }

    def +(m2: TokenMatcher): TokenMatcher = {
      Con(this, m2)
    }
  }

  def oneOrZero(matcher: TokenMatcher): TokenMatcher = matcher or epsilon

  case class Con(m1: TokenMatcher, m2: TokenMatcher) extends TokenMatcher {
    def apply(scanner: Scanner): Try[(List[Token], Scanner)] = {
      val r1 = m1(scanner)
      if (r1.isFailure) {
        r1
      } else {
        val r2 = m2(r1.get._2)
        if (r2.isSuccess) {
          Success(r1.get._1 ::: r2.get._1, r2.get._2)
        } else {
          r2
        }
      }
    }

    override def toString: String = {
      m1.toString + " => " + m2.toString
    }
  }

  case class byType(tokenType: TokenType) extends TokenMatcher {
    def apply(scanner: Scanner) : Try[(List[Token], Scanner)] = {
      val (token, next) = scanner.nextToken
      if (token.tokenType == tokenType) {
        Success((token :: Nil, next))
      } else {
        Failure(new Exception(s"expected token: ${tokenType.toString}, actual token: ${token.toString}"))
      }
    }

    override def toString: String = tokenType.toString
  }
  case class byText(txt: String) extends TokenMatcher {
    def apply(scanner: Scanner) : Try[(List[Token], Scanner)] = {
      val (token, next) = scanner.nextToken
      if (token.txt == txt) {
        Success((token :: Nil, next))
      } else {
        Failure(new Exception(s"expected token: ${txt}, actual token: ${token.toString}"))
      }
    }

    override def toString: String = txt
  }

  case object epsilon extends TokenMatcher{
    def apply(scanner: Scanner) : Try[(List[Token], Scanner)] = Success((List(), scanner))
    override def toString: String = "<epsilon>"
  }
}