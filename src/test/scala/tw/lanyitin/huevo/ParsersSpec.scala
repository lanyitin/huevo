package tw.lanyitin.huevo.parse

import org.scalatest._
import tw.lanyitin.huevo.lex.Scanner
import tw.lanyitin.common.ast._
import tw.lanyitin.common.ast.TokenType._
import tw.lanyitin.common.parser.ParseActionOps._
import scala.language.implicitConversions
import tw.lanyitin.huevo.parse.Ops._
class ParsersSpec extends FlatSpec with Matchers {
    "Or Combinator" should "return second result if the first failed" in {
        or(BooleanAndToken)(BooleanOrToken).run(Scanner("or")) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => if (result.result.txt != "or" || result.state.nextToken._1.tokenType != EOFToken) {
                fail()
            }
        }
    }

    "Or Combinator" should "return first result if the first successed" in {
        or(BooleanAndToken)(BooleanOrToken).run(Scanner("and")) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => if (result.result.txt != "and" || result.state.nextToken._1.tokenType != EOFToken) {
                fail()
            }
        }
    }

    "Parse Boolean Expression" should "able recognize boolean constant true" in {
        val content = "true"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case BooleanLiteralExpression(_, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able recognize boolean constant false" in {
        val content = "false"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case BooleanLiteralExpression(_, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able recognize identifier" in {
        val content = "a1"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case IdentifierExpression(_, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able recognize or operation with two boolean literal" in {
        val content = "true or false"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(_, _, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able recognize and operation with two boolean literal" in {
        val content = "true and false"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(_, _, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able recognize and operation with two identifier" in {
        val content = "a1 and a2"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(_, _, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able recognize and operation wrapped boolean expression" in {
        val content = "(a1 or a2) and (a3 or a4)"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(_, _, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able recognize and operation with arithmatic expression" in {
        val content = "a1 > a2 and a3 < a4"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(_, _, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able recognize and operation wrapped arithmatic expression" in {
        val content = "(a1 > a2) and (a3 < a4)"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(_, _, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able recognize multiple boolean operator expression" in {
        val content = "(a1 > a2) and (a3 < a4) or a1 > a4"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(_, _, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able recognize function call expression" in {
        val content = "func1(a1, a2, a3) and a4"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(_, fc, _) => fc match {
                    case FunctionCallExpression(_, _@_*) => if (result.state.nextToken._1.tokenType != EOFToken) {
                        fail()
                    }
                    case e => fail(e.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Aith Expression" should "able recognize integer literal expression" in {
        val content = "1"
        val scanner = Scanner(content)
        Parsers.parse_arith_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case IntegerLiteralExpression(_, theValue) => if (theValue != 1 || result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Aith Expression" should "able recognize float literal expression" in {
        val content = "1.0"
        val scanner = Scanner(content)
        Parsers.parse_arith_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case FloatLiteralExpression(_, theValue) => if (theValue != 1.0 || result.state.nextToken._1.tokenType != EOFToken) {
                    fail()
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Aith Expression" should "able recognize add operator" in {
        val content = "1.0 + 1"
        val scanner = Scanner(content)
        Parsers.parse_arith_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(token, _, _) => if (token.txt != "+" || result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Aith Expression" should "able recognize minus operator" in {
        val content = "1.0 - 1"
        val scanner = Scanner(content)
        Parsers.parse_arith_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(token, _, _) => if (token.txt != "-" || result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Aith Expression" should "able recognize division operator" in {
        val content = "1.0 / 1"
        val scanner = Scanner(content)
        Parsers.parse_arith_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(token, _, _) => if (token.txt != "/" || result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Aith Expression" should "able recognize multiply operator" in {
        val content = "1.0 * 1"
        val scanner = Scanner(content)
        Parsers.parse_arith_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(token, _, _) => if (token.txt != "*" || result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Aith Expression" should "able recognize multiply operator with parathezied operands" in {
        val content = "(1.0 + 1) * (1 - 1)"
        val scanner = Scanner(content)
        Parsers.parse_arith_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(token, _, _) => if (token.txt != "*" || result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Aith Expression" should "able recognize identifier" in {
        val content = "a1"
        val scanner = Scanner(content)
        Parsers.parse_arith_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case IdentifierExpression(token, _) => if (token.txt != "a1" || result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Aith Expression" should "able recognize add operation with one number literal and on identifier as operand" in {
        {
            val content = "a1 + 1"
            val scanner = Scanner(content)
            Parsers.parse_arith_expression(scanner) match {
                case Left(errors) => fail(errors.mkString("\n"))
                case Right(result) => result.result match {
                    case OperationCallExpression(token, _, _) => if (token.txt != "+" || result.state.nextToken._1.tokenType != EOFToken) {
                        fail(result.toString)
                    }
                    case e => fail(e.toString)
                }
            }
        }
        {
            val content = "1 + a1"
            val scanner = Scanner(content)
            Parsers.parse_arith_expression(scanner) match {
                case Left(errors) => fail(errors.mkString("\n"))
                case Right(result) => result.result match {
                    case OperationCallExpression(token, _, _) => if (token.txt != "+" || result.state.nextToken._1.tokenType != EOFToken) {
                        fail(result.toString)
                    }
                    case e => fail(e.toString)
                }
            }
        }
    }

    "Parse Aith Expression" should "able recognize multiply operation with one number literal and on identifier as operand" in {
        {
            val content = "a1 * 1"
            val scanner = Scanner(content)
            Parsers.parse_arith_expression(scanner) match {
                case Left(errors) => fail(errors.mkString("\n"))
                case Right(result) => result.result match {
                    case OperationCallExpression(token, _, _) => if (token.txt != "*" || result.state.nextToken._1.tokenType != EOFToken) {
                        fail(result.toString)
                    }
                    case e => fail(e.toString)
                }
            }
        }
        {
            val content = "1 * a1"
            val scanner = Scanner(content)
            Parsers.parse_arith_expression(scanner) match {
                case Left(errors) => fail(errors.mkString("\n"))
                case Right(result) => result.result match {
                    case OperationCallExpression(token, _, _) => if (token.txt != "*" || result.state.nextToken._1.tokenType != EOFToken) {
                        fail(result.toString)
                    }
                    case e => fail(e.toString)
                }
            }
        }
    }

    "Parse Aith Expression" should "able recognize add operation with one function call as operand" in {
        val content = " 1 + func1(a1, a2, a3)"
        val scanner = Scanner(content)
        Parsers.parse_arith_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(token, _, _) => if (token.txt != "+" || result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse PropertyCallExpression" should "able recognize simple field call expression" in {
        val content = "a1.a2"
        val scanner = Scanner(content)
        Parsers.parse_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case FieldCallExpression(_, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse PropertyCallExpression" should "able recognize simple method call expression" in {
        val content = "a1.a2()"
        val scanner = Scanner(content)
        Parsers.parse_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case MethodCallExpression(_, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse PropertyCallExpression" should "able recognize nested property call expression" in {
        val content = "a1.a2.a3.a4"
        val scanner = Scanner(content)
        Parsers.parse_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case FieldCallExpression(_, _) => if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parse Boolean Expression" should "able to recognize complex expression" in {
        val content = "NB01.MORE_OTP==Y and MTK1_1.VER==02"
        val scanner = Scanner(content)
        Parsers.parse_boolean_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case OperationCallExpression(token, _, _) => if (token.txt != "and" || result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }


    "Parser" should "able to recognize identifier expression" in {
        val content = "NB01"
        val scanner = Scanner(content)
        Parsers.parse_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case IdentifierExpression(token, _) => if (token.txt != "NB01" || result.state.nextToken._1.tokenType != EOFToken) {
                    fail(result.toString)
                }
                case e => fail(e.toString)
            }
        }
    }

    "Parser" should "able to recognize function call expression" in {
        val content = "NB01(a1, a2)"
        val scanner = Scanner(content)
        Parsers.parse_expression(scanner) match {
            case Left(errors) => fail(errors.mkString("\n"))
            case Right(result) => result.result match {
                case FunctionCallExpression(declaration, _, _) => if (declaration.token.txt != "NB01") {
                    fail(result.toString)
                } else if (result.state.nextToken._1.tokenType != EOFToken) {
                    fail(s"unexpected token ${result.state.nextToken._1}")
                }
                case e => fail(e.toString)
            }
        }
    }

    "A Parse" should "be able to parse complex expression" in {
        val content = "a + b + (1 * 1 / 1) - 1"
        val scanner = Scanner(content)
        val result = Parsers.parse_expression(scanner)
        if (result.isLeft) {
            fail(result.left.get.toString)
        } else if (result.right.get.state.nextToken._1.tokenType != EOFToken) {
            fail(s"${result.right.get.result.toString} ${result.right.get.state.nextToken._1.toString}")
        }
    }

    "a parser" should "be able to parse function definition" in {
        val content = """
                        |def add(a: Float, b: Float): Float = {
                        |  a + b + (1 * 1 / 1) - 1
                        |}
                      """.stripMargin('|').trim()
        val scanner = Scanner(content)
        val result = Parsers.parse_expression(scanner)
        if (result.isLeft) {
            fail(result.left.get.toString)
        } else if (result.right.get.state.nextToken._1.tokenType != EOFToken) {
            fail(s"${result.right.get.result.toString} ${result.right.get.state.nextToken._1.toString}")
        }
    }

     "A Parser" should "be able to parse comment starts with #" in {
         val content = """
                         |#this is a comment
                         |1 + 1 * 1 / 1
                       """.stripMargin('|').trim()
         val scanner = Scanner(content)
         val result = Parsers.parse_expression(scanner)
         if (result.isLeft) {
             fail(result.left.get.toString)
            } else if (result.right.get.state.nextToken._1.tokenType != EOFToken) {
                fail(s"${result.right.get.result.toString} ${result.right.get.state.nextToken._1.toString}")
            }
     }

     "A Parser" should "be able to parse if expression without else" in {
         val content = """
                         |def validate(a:Integer, b: Integer): Boolean = if (a > b) {true}
                       """.stripMargin('|').trim()
         val scanner = Scanner(content)
         val result = Parsers.parse_expression(scanner)
         if (result.isLeft) {
             fail(result.left.get.toString)
            } else if (result.right.get.state.nextToken._1.tokenType != EOFToken) {
                fail(s"${result.right.get.result.toString} ${result.right.get.state.nextToken._1.toString}")
            }
     }

     "A Parser" should "be able to parse if else" in {
         val content = """
                         |def test2(a: Integer, b: Integer): Integer =
                         |if (a > b) {
                         |  1 + 1
                         |} else {
                         |  2 + 1
                         |}
                       """.stripMargin('|').trim()
         val scanner = Scanner(content)
         val result = Parsers.parse_program(scanner)
         if (result.isLeft) {
             fail(result.left.get.toString)
            } else if (result.right.get.state.nextToken._1.tokenType != EOFToken) {
                fail(s"${result.right.get.result.toString}\n${result.right.get.state.nextToken._1.toString}\n${result.right.get.state.toString}")
            }
     }

     "A Parser" should "be able to parse if else if" in {
         val content = """
                         |let a: Integer = 2
                         |let b: Integer = 1
                         |if (a > b) {
                         |  1 + 1
                         |  2 + 2
                         |} else if (a == b) {
                         |  2 + 1
                         |} else {
                         |  4
                         |}
                       """.stripMargin('|').trim()
         val scanner = Scanner(content)
         val result = Parsers.parse_program(scanner)
         if (result.isLeft) {
             fail(result.left.get.toString)
        } else if (result.right.get.state.nextToken._1.tokenType != EOFToken) {
            fail(s"${result.right.get.result.toString}\n${result.right.get.state.nextToken._1.toString}\n${result.right.get.state.state.toString}")
        }
     }

     "Parse Boolean Expression" should "be able to parse property call expression" in {
        val content = """
                        |NB01.M_OPT==Y and MTK1_1.OPT_STUS==Y
                      """.stripMargin('|').trim()
        val scanner = Scanner(content)
        val result = Parsers.parse_expression(scanner)
        if (result.isLeft) {
            fail(result.left.get.toString)
        } else if (result.right.get.state.nextToken._1.tokenType != EOFToken) {
            fail(s"${result.right.get.result.toString} ${result.right.get.state.nextToken._1.toString}")
        }
    }
}
