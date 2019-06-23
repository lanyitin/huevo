package tw.lanyitin.common.parser

import tw.lanyitin.common.ast.Token
import tw.lanyitin.huevo.lex.Scanner


case class ParseResult[+R](state: Scanner, result: R)
case class ParseError(content: String, token: Token, msg: String, committed: Boolean = false) {
  override def toString: String = {
    val lines = content.split("(\r\n|\n\r|\r|\n)")
    (((0 to lines.length - 1).zip(lines).map(t => s"${t._1} ${t._2}").mkString("\n")) ::
    (lines(token.line)) ::
    (" " * token.col + "* " + msg + "\n") :: Nil).mkString("\n")
  }
}
case class ParseAction[+R](run: Scanner => Either[List[ParseError], ParseResult[R]]) {

  def flatMap[B](f: R => ParseAction[B]): ParseAction[B] = ParseActionOps.bind(this)(f)

  def map[B](f: R => B): ParseAction[B] = ParseActionOps.map(this)(f)

  def or[B <: C, C >: R](p2: ParseAction[B]): ParseAction[C] = ParseActionOps.or[R, B, C](this)(p2)

  def andThen[B](p2: ParseAction[B]): ParseAction[B] = flatMap(_ => p2)

  def and[B] (p2: ParseAction[B]): ParseAction[(R, B)] = ParseActionOps.product(this)(p2)

  def either[B](p2: ParseAction[B]): ParseAction[Either[R, B]] = ParseActionOps.either(this)(p2)

  def guard[B](p2: ParseAction[B]): ParseAction[Either[Unit, (R, B)]] = ParseAction[Either[Unit, (R, B)]](s => {

    val result1 = this.run(s)
    result1 match {
      case Right(ParseResult(state1, r)) =>
        val result2 = p2.run(state1)
        result2 match {
          case Right(ParseResult(state2, b)) => Right(ParseResult(state2, Right(r, b)))
          case Left(value) => Left(value)
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
    result <- p.run(s)
    result2 <- f(result.result).run(result.state)
  } yield result2)

  def product[R, B](p1: ParseAction[R])(p2: ParseAction[B]): ParseAction[(R, B)] = ParseAction(state0 => for {
    result1 <- p1.run(state0)
    result2 <- p2.run(result1.state)
  } yield ParseResult(result2.state, (result1.result, result2.result)))

  def map2[R, B, C](p1: ParseAction[R], p2: ParseAction[B], f: (R, B) => C): ParseAction[C] = ParseAction(state0 => for {
    result1 <- p1.run(state0)
    result2 <- p2.run(result1.state)
  } yield ParseResult(result2.state, f(result1.result, result2.result)))

  def many[R](p: ParseAction[R], acc: List[R] = Nil): ParseAction[List[R]] = ParseAction(s => {
    p.run(s) match {
      case Right(result) => many(p,  result.result :: acc).run(result.state)
      case Left(_) => Right(ParseResult(s, acc.reverse))
    }
  })

  def oneOrMore[R](p: ParseAction[R]): ParseAction[List[R]] = ParseAction(s => for {
    result1 <- p.run(s)
    resultRest <- many(p).run(result1.state)
  } yield ParseResult(resultRest.state, result1.result :: resultRest.result))

  def either[R, B](p1: ParseAction[R])(p2: ParseAction[B]): ParseAction[Either[R, B]] = ParseAction[Either[R, B]](state0 => {
    p1.run(state0) match {
      case Right(result) => Right(ParseResult(result.state, Left(result.result)))
      case Left(e1) => p2.run(state0) match {
        case Right(result2) => Right(ParseResult(result2.state, Right(result2.result)))
        case Left(e2) => Left(e1 ++ e2)
      }
    }
  })

  def or[R <: C, B <: C, C](p1: ParseAction[R])(p2: ParseAction[B]): ParseAction[C] = ParseAction[C](state0 => {
    p1.run(state0) match {
      case Right(result) => Right(ParseResult(result.state, result.result))
      case Left(e1) =>
        if (e1.head.committed) {
          Left(e1)
        } else {
          p2.run(state0) match {
            case Right(result2) => Right(ParseResult(state = result2.state, result = result2.result))
            case Left(e2) => Left(e1 ++ e2)
          }
        }
    }
  })

  def wrap[R, B](p1: ParseAction[R])(p2: ParseAction[B]): ParseAction[(B, R, B)] = ParseAction(s => for {
    result1 <- p2.run(s)
    result2 <- p1.run(result1.state)
    result3 <- p2.run(result2.state)
  } yield ParseResult(result3.state, (result1.result, result2.result, result3.result)))

  def wrap2[R, B, C](p1: ParseAction[R])(p2: ParseAction[B], p3: ParseAction[C]): ParseAction[(B, R, C)] = ParseAction(s => for {
    result1 <- p2.run(s)
    result2 <- p1.run(result1.state)
    result3 <- p3.run(result2.state)
  } yield ParseResult(result3.state, (result1.result, result2.result, result3.result)))

  def optionalWrapBy[A, B, C](p1: ParseAction[A])(p2: ParseAction[B], p3: ParseAction[C]): ParseAction[A] = ParseAction[A](s => {
    or(wrap2(p1)(p2, p3).map(v => v._2))(p1).run(s)
  })

  def optional[A](p1: ParseAction[A]): ParseAction[Option[A]] = ParseAction(s => {
    val result = p1.run(s)
    result match {
      case Left(_) => Right(ParseResult(s, None))
      case Right(r) => Right(ParseResult(r.state, Some(r.result)))
    }
  })

  def commit[A](p1: ParseAction[A]): ParseAction[A] = ParseAction(s => {
    val result = p1.run(s)
    result match {
      case Left(errors) => Left(errors.map(error => ParseError(s.content, error.token, error.msg, committed = true)))
      case r@Right(_) => r
    }
  })

  def log[A](p1: ParseAction[A], log: String = ""): ParseAction[A] = ParseAction(s => {
    val nextToken = s.nextToken._1
    val lines = s.content.split("(\r\n|\n\r|\r|\n)")
    println((0 to lines.length - 1).zip(lines).map(t => s"${t._1} ${t._2}").mkString("\n"))
    println(lines(nextToken.line))
    println(" " * nextToken.col + "* " + log)

    val result = p1.run(s)
    result match {
      case Left(errors) => Left(errors)
      case r@Right(_) => r
    }
  })

}
