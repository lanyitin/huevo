package tw.lanyitin.common.parser

import tw.lanyitin.huevo.lex.Scanner

case class ParseResult[R](val state: Scanner, result: R)
case class ParseAction[R, ParseError](run: Scanner => Either[List[ParseError], ParseResult[R]])

object ParseActionOps {
  def map[R, B, ParseError](p: ParseAction[R, ParseError], f: R => B): ParseAction[B, ParseError] = ParseAction(s => for {
    result <- p.run(s).right
  } yield ParseResult(result.state, f(result.result)))  

  def bind[R, B, ParseError](p: ParseAction[R, ParseError], f: R => ParseAction[B, ParseError]): ParseAction[B, ParseError] = ParseAction(s => for {
    result <- p.run(s);
    result2 <- f(result.result).run(result.state)
  } yield result2)

  def flatMap = bind _

  def product[R, B, ParseError](p1: ParseAction[R, ParseError], p2: ParseAction[B, ParseError]): ParseAction[(R, B), ParseError] = ParseAction(state0 => for {
    result1 <- p1.run(state0);
    result2 <- p2.run(result1.state)
  } yield ParseResult(result2.state, (result1.result, result2.result)))

  def map2[R, B, C, ParseError](p1: ParseAction[R, ParseError], p2: ParseAction[B, ParseError], f: (R, B) => C): ParseAction[C, ParseError] = ParseAction(state0 => for {
    result1 <- p1.run(state0);
    result2 <- p2.run(result1.state)
  } yield ParseResult(result2.state, f(result1.result, result2.result)))

  def many[R, ParseError](p: ParseAction[R, ParseError], acc: List[R] = List()): ParseAction[List[R], ParseError] = ParseAction(s => {
    p.run(s) match {
      case Right(result) => many(p,  result.result :: acc).run(result.state)
      case Left(_) => Right(ParseResult(s, acc.reverse))
    }
  })

  def or[R, B, ParseError](p1: ParseAction[R, ParseError], p2: ParseAction[B, ParseError]): ParseAction[Either[R, B], ParseError] = ParseAction(state0 => {
    p1.run(state0) match {
      case Right(result) => Right(ParseResult(result.state, Left(result.result)))
      case Left(e1) => p2.run(state0) match {
        case Right(result2) => Right(ParseResult(result2.state, Right(result2.result)))
        case Left(e2) => Left(e1 ++ e2)
      }
    }
  })

}