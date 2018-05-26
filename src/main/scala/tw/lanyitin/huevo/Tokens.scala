package tw.lanyitin.huevo

import scala.util.{Try, Success, Failure}

object TokenType extends Enumeration {
  type TokenType = Value
  val SpaceToken = Value("<space>")
  val NewLineToken = Value("<newline>")
  val EOFToken = Value("<eof>")
  val UnexpectedToken = Value("UnexpectedToken")
  val NumberToken = Value("<number>")
  val StringToken = Value("<string>")
  val LParanToken = Value("(")
  val RParanToken = Value(")")
  val LCurlyBracket = Value("{")
  val RCurlyBracket = Value("}")
  val CommaToken = Value(",")
  val ColumnToken = Value(":")
  val DefToken = Value("def")
  val IfToken = Value("if")
  val ElseToken = Value("else")
  val IdentifierToken = Value("<identifier>")
  val EqualToken = Value("==")
  val GreaterEqualToken = Value(">=")
  val LessEqualToken = Value("<=")
  val NotEqualToken = Value("!=")
  val GreaterToken = Value(">")
  val LessToken = Value("<")
  val BooleanAndToken = Value("and")
  val BooleanOrToken = Value("or")
  val ArithMultToken = Value("*")
  val ArithDivideToken = Value("/")
  val PlusToken = Value("+")
  val MinusToken = Value("-")
  val AssignToken = Value("=")
  val CommentHeadToken = Value("#")
  val CommentBodyToken = Value("<comment>")
  val QuoteToken = Value("\"")
  val BooleanConstantToken = Value("<boolean>")
// TODO: is it possible to eliminate NotExistToken?
  val NotExistToken = Value("")
}

object Token {
  var tokenId: Integer = 0
  def apply(tokenType: TokenType.TokenType,
            txt: String,
            line: Integer = 0,
            col: Integer = 0): Token = {
    new Token(tokenType, txt, line, col)
  }
}

sealed class Token(val tokenType: TokenType.TokenType,
                   val txt: String,
                   val line: Integer = 0,
                   val col: Integer = 0) {
  val id = Token.tokenId
  Token.tokenId = Token.tokenId + 1

  override def toString: String = {
    val espedTxt = txt.replace("\n", "\\n").replace("\r", "\\r")
    s"${tokenType}('${espedTxt}')"
  }
}
case class NullToken() extends Token(TokenType.NotExistToken, "")

object MatcherGenerator {
  import TokenType._
  trait TokenMatcher {
    def apply(scanner: Scanner): Try[(List[Token], Scanner)]

    def and(m2: TokenMatcher): TokenMatcher = {
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

        override def toString: String =
          "[" + self.toString + "&" + m2.toString + "]"
      }
    }

    def or(m2: TokenMatcher): TokenMatcher = {
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
    def apply(scanner: Scanner): Try[(List[Token], Scanner)] = {
      val (token, next) = scanner.nextToken
      if (token.tokenType == tokenType) {
        Success((token :: Nil, next))
      } else {
        Failure(new Exception(
          s"expected token: ${tokenType.toString}, actual token: ${token.toString}"))
      }
    }

    override def toString: String = tokenType.toString
  }

  implicit def tokenTypeToMatcher(token: TokenType): TokenMatcher = {
    byType(token)
  }

  case object epsilon extends TokenMatcher {
    def apply(scanner: Scanner): Try[(List[Token], Scanner)] =
      Success((List(), scanner))
    override def toString: String = "<epsilon>"
  }
}
