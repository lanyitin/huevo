package tw.lanyitin.huevo

import TokenType._
import ast.Node
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}

object Parser {
  import MatcherGenerator._

  def parse(scanner: Scanner, acc: List[Node] = Nil): Try[(Node, Scanner)] = {
    var acc: List[Node] = Nil
    var final_state: Scanner = scanner
    var error: Throwable = null

    var going = true;
    while(going) {
      var parse_result = parse_expression(final_state)
      if (parse_result.isSuccess) {
          acc = parse_result.get._1 :: acc
          final_state = parse_result.get._2
      } else {
        error = parse_result.failed.get
        going = false
      }
    }
    if (acc.isEmpty) {
      Failure(error)
    } else if (acc.size == 1) {
      Success((acc(0), final_state))
    } else {
      Success((Node(new Token(NotExistToken, ""), acc.reverse), final_state))
    }
  }

  def parse_expression(scanner: Scanner): Try[(Node, Scanner)] = {
    val next_token = scanner.take(1)(0)
    if (next_token.tokenType == DefToken) {
      parse_function_defintion(scanner)
    } else if (next_token.tokenType == IdentifierToken || next_token.tokenType == NumberToken){
      parse_arith_expression(scanner)
    } else if (next_token.tokenType == IfToken) {
      parse_if_expression(scanner)
    } else if (next_token.tokenType == CommentBodyToken) {
      parse_expression(scanner.skip({var a = 2; (t: Token) => {a=a-1; a > 0}}))
    } else {
      Failure(new Exception(s"unexpected token ${next_token.tokenType.toString}"))
    }
  }

  def parse_if_expression(scanner: Scanner): Try[(Node, Scanner)] = {
    // accept if (
    expect(scanner, byType(IfToken) + byType(LParanToken)).flatMap(r1 => {
      val (tokens: List[Token], scanner2: Scanner) = r1
      val first_token = tokens(0)
      // parse condition
      parse_boolean_expression(scanner2).flatMap(r2 => {
        val (condition: Node, scanner3: Scanner) = r2
        expect(scanner3, byType(RParanToken) + byType(LCurlyBracket)).flatMap(r3 => {
          val (_, scanner4: Scanner) = r3
          parse(scanner4).flatMap(r4 => {
            val (true_path: Node, scanner5: Scanner) = r4
            expect(scanner5, byType(RCurlyBracket) + oneOrZero(byType(ElseToken) + oneOrZero(byType(LCurlyBracket)))).flatMap(r5 => {
              val (tokens: List[Token], scanner6: Scanner) = r5
              if (tokens.size == 3) {
                parse(scanner6).flatMap(r6 => {
                  val (false_path: Node, scanner7) = r6
                  expect(scanner7, byType(RCurlyBracket)).flatMap(r7 => {
                    val (_, scanner8: Scanner) = r7
                    Success((Node(first_token, condition :: true_path :: false_path :: Nil), scanner8))
                  })
                })
              } else if (tokens.size == 2) {
                parse_if_expression(scanner6).flatMap(r6 => {
                  val (false_path: Node, scanner7: Scanner) = r6
                  Success((Node(first_token, condition :: true_path :: false_path :: Nil), scanner7))
                })
              } else {
                  Success((Node(first_token, condition :: true_path :: Nil), scanner6))
              }
            })
          })
        })
      })
    })
  }

  def parse_boolean_expression(scanner: Scanner): Try[(Node, Scanner)] = {
    parse_boolean_term(scanner).flatMap(r1 => {
      val (boolean_term: Node, scanner2: Scanner) = r1
      expect(scanner2, (byType(OperatorToken) and (byText("and") or byText("or")))).flatMap(r2 => {
        val (tokens: List[Token], scanner3: Scanner) = r2
        parse_boolean_expression(scanner3).flatMap(r3 => {
          val (expr2: Node, scanner4: Scanner) = r3
          Success((Node(tokens(0), boolean_term :: expr2 :: Nil), scanner4))
        })
      }).orElse(Success((boolean_term, scanner2)))
    })
  }

  def parse_boolean_term(scanner: Scanner): Try[(Node, Scanner)] = {
    parse_arith_expression(scanner).flatMap(r1 => {
      val (expr1: Node, scanner2: Scanner) = r1
      expect(scanner2, (byType(OperatorToken) and (byText(">") or byText("<") or byText(">=") or byText("<=") or byText("==")))).flatMap(r2 => {
        val (tokens: List[Token], scanner3: Scanner) = r2
        parse_arith_expression(scanner3).flatMap(r3 => {
          val (expr2: Node, scanner4: Scanner) = r3
          Success((Node(tokens(0), expr1 :: expr2 :: Nil), scanner4))
        })
      })
    }).orElse({
      expect(scanner, (byType(IdentifierToken) or byType(BooleanConstantToken))).flatMap(r1 => {
        val (tokens: List[Token], scanner2: Scanner) = r1
        Success(Node(tokens(0), Nil), scanner2)
      })
    })
  }

  def parse_function_defintion(scanner: Scanner): Try[(Node, Scanner)] = {
    parse_function_declaration(scanner).flatMap(r1 => {
      val (fun_decl: Node, scanner2: Scanner) = r1
      expect(scanner2, (byType(OperatorToken) and byText("=")) + (byType(LCurlyBracket))).flatMap(r2 => {
        val (tokens: List[Token], scanner3: Scanner) = r2
        parse(scanner3).flatMap(r3 => {
          val (expressions: Node, scanner4: Scanner) = r3
          expect(scanner4, byType(RCurlyBracket)).flatMap(r4 => {
            val (_, scanner5: Scanner) = r4
            Success((Node(tokens(0), List(fun_decl, expressions)), scanner5))
          })
        })
      })
    })
  }

  def parse_function_declaration(scanner: Scanner): Try[(Node, Scanner)] = {
    var matcher = byType(DefToken) + byType(IdentifierToken) + byType(LParanToken)
    expect(scanner, matcher).flatMap(r1 => {
      val (tokens: List[Token], scanner2: Scanner) = r1
      val id_token = tokens(1)
      parse_argument_list(scanner2).flatMap(r2 => {
        val (arg_list: Node, scanner3: Scanner) = r2
        expect(scanner3, byType(RParanToken) + byType(ColumnToken)).flatMap(r3 => {
          val (_, scanner4: Scanner) = r3
          parse_type(scanner4).flatMap(r4 => {
            val (fun_type: Node, scanner5: Scanner) = r4
            Success((Node(id_token, List(arg_list, fun_type)), scanner5))
          })
        })
      })
    })
  }

  def parse_type(scanner: Scanner): Try[(Node, Scanner)] = {
    byType(IdentifierToken)(scanner).flatMap(r => {
      Success((Node(r._1(0), Nil), r._2))
    })
  }

  def parse_argument_list(scanner: Scanner): Try[(Node, Scanner)] = {
    var matcher = byType(IdentifierToken) + byType(ColumnToken)
    matcher(scanner).flatMap(r1 => {
        val (tokens: List[Token], scanner2: Scanner) = r1
        parse_type(scanner2).flatMap(r2 => {
          val (type_node: Node, scanner3: Scanner) = r2
          val (rest_args, scanner4) = parse_rest_arg_list(scanner3)
          Success(Node(new Token(NotExistToken, ""), Node(tokens(0), type_node::Nil) :: rest_args), scanner4)
        })
    })
  }

  def parse_rest_arg_list(scanner: Scanner, acc: List[Node] = Nil): (List[Node], Scanner) = {
    var matcher = byType(CommaToken) + byType(IdentifierToken) + byType(ColumnToken)
    expect(scanner, matcher).flatMap(r1 => {
      val (tokens: List[Token], scanner2: Scanner) = r1
      parse_type(scanner2).flatMap(r2 => {
        val (type_node: Node, scanner3: Scanner) = r2
        Success(parse_rest_arg_list(scanner3,  Node(tokens(1), type_node :: Nil) :: acc))
      })
    }).orElse(Success((acc.reverse, scanner))).get
  }


  def parse_arith_expression(scanner: Scanner): Try[(Node, Scanner)] = {
    parse_arith_term(scanner).flatMap(r1 => {
      val (arith_term: Node, scanner2: Scanner) = r1
      val matcher = (byType(OperatorToken) and (byText("+") or byText("-")))
      matcher(scanner2).flatMap( r2 => {
        val (token: List[Token], scanner3: Scanner) = r2
        parse_arith_expression(scanner3).flatMap(r3 => {
          val (arith_exp: Node, scanner4: Scanner) = r3
          Success(Node(token(0), List(arith_term, arith_exp)), scanner4)
        })
      }).orElse(Success((arith_term, scanner2)))
    })
  }

  def parse_arith_term(scanner: Scanner): Try[(Node, Scanner)] = {
    parse_arith_factor(scanner).flatMap( (r1) => {
      val (arith_factor: Node, scanner2: Scanner) = r1
      val matcher = (byType(OperatorToken) and (byText("*") or byText("/")))
      matcher(scanner2).flatMap( (r2) => {
        val (op_token, scanner3) = r2
        parse_arith_term(scanner3).flatMap( (r3) => {
          val (arith_term: Node, scanner4: Scanner) = r3
          Success((Node(op_token(0), arith_factor :: arith_term :: Nil), scanner4))
        } )
      }).orElse(Success((arith_factor, scanner2)))
    })
  }

  def parse_arith_factor(scanner: Scanner): Try[(Node, Scanner)] = {
    val (token, scanner2) = scanner.nextToken
    token.tokenType match {
      case NumberToken | IdentifierToken => Success((Node(token, Nil), scanner2))
      case LParanToken => {
        parse_arith_expression(scanner2).flatMap(r1 => {
          val (arith_exp: Node, scanner3: Scanner) = r1
          expect(scanner3, byType(RParanToken)).flatMap(r2 => {
            val (token: List[Token], scanner4: Scanner) = r2
            Success((arith_exp, scanner4))
          })
        })
      }
    }
  }

  def expect(scanner: Scanner, matcher: TokenMatcher): Try[(List[Token], Scanner)] = {
    val match_result = matcher(scanner)
    if (match_result.isFailure) {
      match_result
    } else {
      Success((match_result.get._1, match_result.get._2))
    }
  }
}