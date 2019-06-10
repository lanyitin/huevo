package tw.lanyitin.huevo.lex

import scala.util.{Try, Success, Failure}
import tw.lanyitin.common.ast._

object MatcherGenerator {
  import tw.lanyitin.common.ast.TokenType._
  import tw.lanyitin.common.ast.{Token, NullToken, TokenType}
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

  import scala.language.implicitConversions
  implicit def tokenTypeToMatcher(token: TokenType): TokenMatcher = {
    byType(token)
  }

  case object epsilon extends TokenMatcher {
    def apply(scanner: Scanner): Try[(List[Token], Scanner)] =
      Success((List(), scanner))
    override def toString: String = "<epsilon>"
  }
}
