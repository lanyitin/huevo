package tw.lanyitin.common.parser

import tw.lanyitin.huevo.lex.Scanner
import tw.lanyitin.common.ast.Token


case class ParseResult[+R](val state: Scanner, result: R)
case class ParseError(token: Token, msg: String)
case class ParseAction[+R](run: Scanner => Either[List[ParseError], ParseResult[R]]) {

  def flatMap[B](f: R => ParseAction[B]) = ParseActionOps.bind(this)(f)

  def map[B](f: R => B) = ParseActionOps.map(this)(f)

  def and[B] (p2: ParseAction[B]) = ParseActionOps.product(this)(p2)

  def or[B](p2: ParseAction[B]) = ParseActionOps.or(this)(p2)

  def orM[B <: C, C >: R](p2: ParseAction[B]) = ParseAction[C](state0 => {
    this.run(state0) match {
      case Right(result) => Right(ParseResult(result.state, result.result))
      case Left(e1) => p2.run(state0) match {
        case Right(result2) => Right(ParseResult(result2.state, result2.result))
        case Left(e2) => Left(e1 ++ e2)
      }
    }
  })

  def guard[B](p2: ParseAction[B]): ParseAction[Either[Unit, (R, B)]] = ParseAction[Either[Unit, (R, B)]](s => {
    
    val result1 = this.run(s)
    result1 match {
      case Right(ParseResult(state1, r)) => {
        val result2 = p2.run(state1)
        result2 match {
          case Right(ParseResult(state2, b)) => Right(ParseResult(state2, Right(r, b)))
          case Left(value) => Left(value)
        }
      }
      case Left(_) => Right(ParseResult(s, Left(())))
    }
  })
}

object ParseActionOps {

  def map[R, B](p: ParseAction[R])(f: R => B): ParseAction[B] = ParseAction(s => for {
    result <- p.run(s).right
  } yield ParseResult(result.state, f(result.result)))  

  def bind[R, B](p: ParseAction[R])(f: R => ParseAction[B]): ParseAction[B] = ParseAction(s => for {
    result <- p.run(s);
    result2 <- f(result.result).run(result.state)
  } yield result2)

  def product[R, B](p1: ParseAction[R])(p2: ParseAction[B]): ParseAction[(R, B)] = ParseAction(state0 => for {
    result1 <- p1.run(state0);
    result2 <- p2.run(result1.state)
  } yield ParseResult(result2.state, (result1.result, result2.result)))

  def map2[R, B, C](p1: ParseAction[R], p2: ParseAction[B], f: (R, B) => C): ParseAction[C] = ParseAction(state0 => for {
    result1 <- p1.run(state0);
    result2 <- p2.run(result1.state)
  } yield ParseResult(result2.state, f(result1.result, result2.result)))

  def many[R](p: ParseAction[R], acc: List[R] = Nil): ParseAction[List[R]] = ParseAction(s => {
    p.run(s) match {
      case Right(result) => many(p,  result.result :: acc).run(result.state)
      case Left(_) => Right(ParseResult(s, acc.reverse))
    }
  })

  def oneOrMore[R](p: ParseAction[R]): ParseAction[List[R]] = ParseAction(s => for {
    result1 <- p.run(s);
    resultRest <- many(p).run(result1.state)
  } yield ParseResult(resultRest.state, result1.result :: resultRest.result))

  def or[R, B](p1: ParseAction[R])(p2: ParseAction[B]): ParseAction[Either[R, B]] = ParseAction[Either[R, B]](state0 => {
    p1.run(state0) match {
      case Right(result) => Right(ParseResult(result.state, Left(result.result)))
      case Left(e1) => p2.run(state0) match {
        case Right(result2) => Right(ParseResult(result2.state, Right(result2.result)))
        case Left(e2) => Left(e1 ++ e2)
      }
    }
  })

  def orM[R <:C, B <: C, C](p1: ParseAction[R])(p2: ParseAction[B]): ParseAction[C] = ParseAction[C](state0 => {
    p1.run(state0) match {
      case Right(result) => Right(ParseResult(result.state, result.result))
      case Left(e1) => p2.run(state0) match {
        case Right(result2) => Right(ParseResult(result2.state, result2.result))
        case Left(e2) => Left(e1 ++ e2)
      }
    }
  })
}