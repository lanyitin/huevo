package tw.lanyitin.huevo.parse

import tw.lanyitin.huevo.lex.Scanner
import tw.lanyitin.common.parser.ParseAction
import tw.lanyitin.common.parser.ParseResult
import tw.lanyitin.common.parser.ParseError
import tw.lanyitin.common.parser.ParseActionOps._
import tw.lanyitin.common.ast._
import tw.lanyitin.common.ast.TokenType._

object Ops {
  import scala.language.implicitConversions

  implicit def tokenTypeToParseAction[T <: TokenType](tokenType: T): ParseAction[Token] = this.expectTokenType(tokenType, token => token)

  implicit def id[R](r: R): R = r

  implicit def extractEither[R](either: Either[R, R]): R = {
    either.fold(id, id)
  }

  def expect[R](predicate: Token => Boolean, gen: Token => R, msgGen: Token => String): ParseAction[R] = ParseAction(s => {
    val (token, nextState) = s.nextToken
    if (predicate(token)) {
      Right(ParseResult(nextState, gen(token)))
    } else {
      Left(List(ParseError(s.content, token, msgGen(token))))
    }
  })

  def expectTokenType[R](tokenType: TokenType, gen: Token => R): ParseAction[R] =
    this.expect(
      token => token.tokenType == tokenType,
      gen,
      _ => s"expected to be ${tokenType.toString}"
    )
}

object Parsers {
  import scala.language.implicitConversions
  import Ops._

  def parse_program(scanner: Scanner): Either[List[ParseError], ParseResult[ExprsBlock]] =
    this.parse_expressions_block(scanner)

  def parse_expressions_block(scanner: Scanner): Either[List[ParseError], ParseResult[ExprsBlock]] = {
    val action = ParseAction(this.parse_expression)
    val case1 = wrap2(commit(oneOrMore(action)))(LCurlyBracket, RCurlyBracket).map(x => ExprsBlock(x._2))
    val case2 = action.map(x => ExprsBlock(List(x)))
    (case1 or case2).run(scanner)
  }

  def parse_identifier_expression(scanner: Scanner): Either[List[ParseError], ParseResult[IdentifierExpression]] =
    expectTokenType(IdentifierToken, IdentifierExpression(_, null)).run(scanner)


  def parse_expression(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val next_tokens = scanner.take(4)
    val next_token = next_tokens.head
    if (next_token.tokenType == LetToken) {
      commit(ParseAction(parse_variable_definition)).run(scanner)
    } else if (next_token.tokenType == DefToken) {
      commit(ParseAction(parse_function_defintion)).run(scanner)
    } else if (next_token.tokenType == IfToken) {
      commit(ParseAction(parse_if_expression)).run(scanner)
    } else if (next_token.tokenType == LCurlyBracket) {
      commit(ParseAction(parse_expressions_block)).run(scanner)
    } else if (next_token.tokenType == CommentBodyToken) {
      parse_expression(scanner.skip(1))
    } else {
      (ParseAction(this.parse_boolean_expression) or ParseAction(this.parse_arith_expression)).run(scanner)
    }
  }

  def parse_variable_definition(scanner: Scanner): Either[List[ParseError], ParseResult[VariableDefinitionExpression]] = for {
    result1 <- LetToken.run(scanner)
    result2 <- expectTokenType(IdentifierToken, Variable(_, null, 0)).run(result1.state)
    result3 <- (ColumnToken andThen ParseAction(parse_type)).run(result2.state)
    result4 <- AssignToken.run(result3.state)

    result4 <- this.parse_expression(result4.state)
  } yield ParseResult(result4.state, VariableDefinitionExpression(result2.result, result4.result))

  def parse_function_call_args(scanner: Scanner): Either[List[ParseError], ParseResult[List[Expression]]] = {
    val exprAfterComma = CommaToken andThen ParseAction(this.parse_expression)
    val parseAction = ParseAction(this.parse_expression) guard many(exprAfterComma)
    (parseAction map ((r: Either[Unit, (Expression, List[Expression])]) => r match {
      case Right((firstExpr, restExpr)) => firstExpr :: restExpr
      case Left(()) => Nil
    })).run(scanner)
  }

  def parse_function_call(scanner: Scanner): Either[List[ParseError], ParseResult[FunctionCallExpression]] = {
    val action = ParseAction(this.parse_identifier_expression) and
      LParanToken and
      ParseAction(this.parse_function_call_args) and
      RParanToken

    action.map(r => {
      FunctionCallExpression(FunctionDeclaration(r._1._1._1.token, null, null), r._1._2:_*)
    }).run(scanner)
  }

  def parse_if_expression(scanner: Scanner): Either[List[ParseError], ParseResult[IfExpression]] = for {
    result1 <- (IfToken and LParanToken and ParseAction(this.parse_boolean_expression) and RParanToken).run(scanner)
    result2 <- (LCurlyBracket and ParseAction(parse_expression) and RCurlyBracket).run(result1.state)
    result3 <- (ElseToken guard (LCurlyBracket and ParseAction(parse_expression) and RCurlyBracket)).run(result2.state) map (either => either.result match {
      case Left(_) => (result2.state, ExprsBlock(Nil))
      case Right(result) => (either.state, result._2._1._2)
    })
  } yield ParseResult(result3._1, IfExpression(result1.result._1._2, result2.result._1._2, result3._2))

  def parse_boolean_expression(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    for {
      result1 <- ParseAction(this.parse_boolean_term).run(scanner)
      result2 <- (BooleanOrToken guard commit(ParseAction(this.parse_boolean_term))).run(result1.state)
    } yield result2.result match {
      case Left(()) => result1
      case Right(r2) => ParseResult(result2.state, OperationCallExpression(
        r2._1,
        result1.result,
        r2._2
      ))
    }
  }

  def parse_boolean_term(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    for {
      result1 <- ParseAction(this.parse_boolean_factor).run(scanner)
      result2 <- (BooleanAndToken guard commit(ParseAction(this.parse_boolean_factor))).run(result1.state)
    } yield result2.result match {
      case Left(()) => result1
      case Right(r2) => ParseResult(result2.state, OperationCallExpression(
        r2._1,
        result1.result,
        r2._2
      ))
    }
  }

  def parse_boolean_factor(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val action = ParseAction(this.parse_arith_relation) or
                 ParseAction(this.parse_boolean_literal) or
                 ParseAction(this.parse_identifier_expression)
    val possiblePropertyCallExpr = map2(action, optional(ParseAction(this.parse_selector_chain)), (source: Expression, selectors: Option[List[Either[FunctionCallExpression, IdentifierExpression]]]) => {
      selectors match {
        case None => source
        case Some(ss) => {
          ss.foldLeft(source)(this.generatePropertyCallExpression)
        }
      }
    })

    val booleanExpressionWrapByParan = (LParanToken and commit(ParseAction(this.parse_boolean_expression)) and RParanToken).map(t => {
      t._1._2
    })

    (booleanExpressionWrapByParan or possiblePropertyCallExpr).run(scanner)
  }

  def parse_relation_operators(scanner: Scanner): Either[List[ParseError], ParseResult[Token]] = {
    (GreaterEqualToken or GreaterToken or LessEqualToken or LessToken or EqualToken or NotEqualToken).run(scanner)
  }

  def parse_arith_relation(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val arith_replation_ops = ParseAction(this.parse_relation_operators)
    for {
      result1 <- ParseAction(this.parse_arith_expression).run(scanner)
      result2 <- (arith_replation_ops guard commit(ParseAction(this.parse_arith_expression))).run(result1.state)
    } yield result2.result match {
      case Left(()) => result1
      case Right(r2) => ParseResult(result2.state, OperationCallExpression(
        r2._1,
        result1.result,
        r2._2
      ))
    }
  }

  def parse_arith_expression(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val operators = PlusToken or MinusToken
    var parseTerm = ParseAction(this.parse_arith_term)
    val operation = many(operators and commit(parseTerm))

    (parseTerm and operation).map(t => {
      t._2 match {
        case Nil => t._1
        case lst => {
          t._2.foldLeft(t._1)((source, t2) => OperationCallExpression(t2._1, source, t2._2))
        }
      }
    }).run(scanner)
  }

  def parse_arith_term(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val operators = ArithMultToken or ArithDivideToken or ModToken
    var parseFactor = ParseAction(this.parse_arith_factor)
    val operation = many(operators and commit(parseFactor))

    (parseFactor and operation).map(t => {
      t._2 match {
        case Nil => t._1
        case lst => {
          t._2.foldLeft(t._1)((source, t2) => OperationCallExpression(t2._1, source, t2._2))
        }
      }
    }).run(scanner)
  }

  def parse_arith_factor(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val numberExpr = ParseAction(this.parse_number_literal_expression)

    val identifierOrFunctionCall = map(ParseAction(this.parse_identifier_expression) and (LParanToken guard (ParseAction(this.parse_function_call_args) and RParanToken)))(x => {
      x._2 match {
        case Left(()) => x._1
        case Right(right) => FunctionCallExpression(FunctionDeclaration(x._1.token, null, null), right._2._1: _*)
      }
    })

    val possiblePropertyCallExpr = map2(numberExpr or identifierOrFunctionCall, optional(ParseAction(this.parse_selector_chain)), (source: Expression, selectors: Option[List[Either[FunctionCallExpression, IdentifierExpression]]]) => {
      selectors match {
        case None => source
        case Some(ss) => {
          ss.foldLeft(source)(this.generatePropertyCallExpression)
        }
      }
    })

    val arithExprWrapByParan: ParseAction[Expression] = (LParanToken and commit(ParseAction(this.parse_arith_expression)) and RParanToken).map(x => {
      x._1._2
    })

    (arithExprWrapByParan or possiblePropertyCallExpr).run(scanner)
    // booleanExr.run(scanner)
  }

  def parse_boolean_literal(scanner: Scanner): Either[List[ParseError], ParseResult[BooleanLiteralExpression]] = {
    BooleanConstantToken.map(x => {
      BooleanLiteralExpression(x, x.txt.toBoolean)
    }).run(scanner)
  }

  def parse_number_literal_expression(scanner: Scanner): Either[List[ParseError], ParseResult[NumberLiteralExpression[_]]] = {
    expectTokenType(NumberToken, x => {
      if (x.txt.contains(".")) {
        FloatLiteralExpression(x, x.txt.toFloat)
      } else {
        IntegerLiteralExpression(x, x.txt.toInt)
      }
    }).run(scanner)
  }

  def parse_function_defintion(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val action = ParseAction(this.parse_function_declaration) and AssignToken and  ParseAction(this.parse_expressions_block)
    action.map(x => {
      //  ((tw.lanyitin.common.ast.FunctionDeclaration, tw.lanyitin.common.ast.Token), tw.lanyitin.common.ast.ExprsBlock)
      FunctionDefinitionExpression(x._1._1, x._2)
    }).run(scanner)
  }

  def parse_function_declaration(scanner: Scanner): Either[List[ParseError], ParseResult[FunctionDeclaration]] = {
    val action = DefToken and ParseAction(this.parse_identifier_expression) and LParanToken and ParseAction(this.parse_parameter_list) and RParanToken and optional(ColumnToken andThen(ParseAction(this.parse_type)))
    action.map(x => {
      // ((((Token, IdentifierExpression), Token), List[Parameter]), Token)
      FunctionDeclaration(x._1._1._1._1._2.token, x._1._1._2, null)
    }).run(scanner)
  }

  def parse_parameter_list(scanner: Scanner): Either[List[ParseError], ParseResult[List[Parameter]]] = {
    val action = ParseAction(this.parse_parameter) guard many(CommaToken and ParseAction(this.parse_parameter))
    action.flatMap(value => ParseAction(s => {
      if (value.isLeft) {
        Right(ParseResult(s, Nil))
      } else {
        val result = value.right.get
        Right(ParseResult(s, result._1 :: result._2.map(_._2)))
      }
    })).run(scanner)
  }
  def parse_parameter(scanner: Scanner): Either[List[ParseError], ParseResult[Parameter]] = {
    val action = ParseAction(this.parse_identifier_expression) and ColumnToken and commit(ParseAction(this.parse_type))
    action.map(x => {
      // ((IdentifierExpression, Token), Type)
      Parameter(x._1._1.token, x._2, 0)
    }).run(scanner)
  }

  def parse_type(scanner: Scanner): Either[List[ParseError], ParseResult[Type]] = {
    val mapping: IdentifierExpression => Type = (expr: IdentifierExpression) => {
      expr.token.txt match {
        case "Float" => HFloat
        case "Integer" => HInteger
        case "Boolean" => HBoolean
        case "Unit" => AnyValue
        case a@_ => UnsolveType(a)
      }
    }
    (ParseAction(this.parse_identifier_expression) map mapping).run(scanner)
  }

  def parse_selector(scanner: Scanner): Either[List[ParseError], ParseResult[Either[FunctionCallExpression, IdentifierExpression]]] = {
    (PropertyAccessToken andThen ParseAction(this.parse_function_call).either(ParseAction(this.parse_identifier_expression))).run(scanner)
  }

  def parse_selector_chain(scanner: Scanner): Either[List[ParseError], ParseResult[List[Either[FunctionCallExpression, IdentifierExpression]]]] = {
    oneOrMore(ParseAction(this.parse_selector)).run(scanner)
  }

  def generatePropertyCallExpression(source: Expression, property: Either[FunctionCallExpression, IdentifierExpression]): Expression = {
    property match {
      case Left(expr) => MethodCallExpression(source, expr)
      case Right(expr) => FieldCallExpression(source, expr)
    }
  }


}