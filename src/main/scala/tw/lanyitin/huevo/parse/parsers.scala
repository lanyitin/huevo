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

  def manySapceOrNewline: ParseAction[Unit] = many(expectTokenType(SpaceToken) orM expectTokenType(NewLineToken)) map ({case _ => ()}) 

  def expect[R](predicate: Token => Boolean, gen: Token => R, msgGen: Token => String): ParseAction[R] = ParseAction(s => {
    val (token, nextState) = s.nextToken
    if (predicate(token)) {
      Right(ParseResult(nextState, gen(token)))
    } else {
      Left(List(ParseError(token, msgGen(token))))
    }
  })

  def expectTokenType[R](tokenType: TokenType, gen: Token => R = (token: Token) => token): ParseAction[R] = 
    this.expect(
      token => token.tokenType == tokenType,
      gen,
      token => s"${token.txt} is not a ${tokenType.toString()}"
    )

  def wrap[R, B](p1: ParseAction[R])(p2: ParseAction[B]): ParseAction[(B, R, B)] = ParseAction(s => for {
    result1 <- p2.run(s);
    result2 <- p1.run(result1.state);
    result3 <- p2.run(result2.state)
  } yield ParseResult(result3.state, (result1.result, result2.result, result3.result)))

  implicit def tokenTypeToParseAction[T <: TokenType](tokenType: T): ParseAction[Token] = this.expectTokenType(tokenType)

  implicit def id[R](r: R): R = r

  implicit def extractEither[R](either: Either[R, R]): R = {
    either.fold(id, id)
  }
}

object Parsers {
  import scala.language.implicitConversions
  import Ops._

  def parse_program(scanner: Scanner): Either[List[ParseError], ParseResult[ExprsBlock]] =
    this.parse_expressions_block(scanner)

  def parse_expressions_block(scanner: Scanner): Either[List[ParseError], ParseResult[ExprsBlock]] = {
    val action: ParseAction[ExprsBlock] = map(oneOrMore(ParseAction[Expression](this.parse_expression(_))))(ExprsBlock(_))
    
    (LCurlyBracket guard (action and RCurlyBracket)).flatMap(value => ParseAction(s => {
      // Either[Unit,(Token, (ExprsBlock, Token))]
      if (value.isLeft) {
        action.run(s)
      } else {
        val result = value.right.get
        Right(ParseResult(s, result._2._1))
      }
    })).run(scanner)
  }

  def parse_identifier_expression(scanner: Scanner) = 
    expectTokenType(IdentifierToken, IdentifierExpression(_, null)).run(scanner)


  def parse_expression(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val next_tokens = scanner.take(4)
    val next_token = next_tokens(0)
    if (next_token.tokenType == LetToken) {
      parse_variable_definition(scanner)
    } else if (next_token.tokenType == DefToken) {
      parse_function_defintion(scanner)
    } else if (next_token.tokenType == NumberToken) {
      parse_arith_expression(scanner)
    } else if (next_token.tokenType == BooleanConstantToken) {
      parse_boolean_expression(scanner)
    } else if (next_token.tokenType == IdentifierToken) {
      (next_tokens(1).tokenType) match {
        case PlusToken | MinusToken | ArithDivideToken | ArithMultToken | ModToken =>
          parse_arith_expression(scanner)
        case GreaterEqualToken | GreaterToken | LessEqualToken | LessToken |
            EqualToken | NotEqualToken =>
          parse_boolean_expression(scanner)
        case LParanToken =>
          parse_function_call(scanner)
        case CommaToken | RParanToken | RCurlyBracket | EOFToken | DefToken =>
          parse_identifier_expression(scanner)
      }
    } else if (next_token.tokenType == IfToken) {
      parse_if_expression(scanner)
    } else if (next_token.tokenType == RParanToken) {
      parse_expressions_block(scanner.skip(1))
    } else if (next_token.tokenType == CommentBodyToken) {
      parse_expression(scanner.skip(1))
    } else {
      val current_line = scanner.content.split("\n")(next_token.line)
      Left(List(ParseError(next_token, s"unexpected token ${next_tokens.map(_.txt)}\n${next_token.line + 1} ${current_line}")))
    }
  }


  def parse_variable_definition(scanner: Scanner): Either[List[ParseError], ParseResult[VariableDefinitionExpression]] = for {
    result1 <- LetToken.run(scanner);
    result2 <- expectTokenType(IdentifierToken, Variable(_, null, 0)).run(result1.state);
    result3 <- EqualToken.run(result2.state);
    result4 <- this.parse_expression(result3.state)
  } yield ParseResult(result4.state, VariableDefinitionExpression(result2.result, result4.result))

  def parse_function_call_args(scanner: Scanner): Either[List[ParseError], ParseResult[List[Expression]]] = {
    val exprAfterComma = (expectTokenType(CommaToken) and ParseAction(this.parse_expression)) map (r => r._2)
    val parseAction = ParseAction(this.parse_expression) guard many(exprAfterComma) 
    (parseAction map ((r: Either[Unit, (Expression, List[Expression])]) => r match {
      case Right((firstExpr, restExpr)) => firstExpr :: restExpr
      case Left(()) => Nil
    })).run(scanner)
  }

  def parse_function_call(scanner: Scanner): Either[List[ParseError], ParseResult[FunctionCallExpression]] = {
    val action = ParseAction(this.parse_identifier_expression) and
      expectTokenType(LParanToken) and
      ParseAction(this.parse_function_call_args) and
      expectTokenType(RParanToken)

    action.map(r => {
      FunctionCallExpression(FunctionDeclaration(r._1._1._1.token, null, null), r._1._2:_*)
    }).run(scanner)
  }

  def parse_if_expression(scanner: Scanner): Either[List[ParseError], ParseResult[IfExpression]] = for {
    result1 <- (IfToken and LParanToken and ParseAction(this.parse_boolean_expression(_)) and RParanToken).run(scanner);
    result2 <- (LCurlyBracket and ParseAction(parse_expression(_)) and RCurlyBracket).run(result1.state);
    result3 <- (ElseToken guard (LCurlyBracket and ParseAction(parse_expression(_)) and RCurlyBracket)).run(result2.state) map (either => either.result match {
      case Left(_) => (result2.state, ExprsBlock(Nil))
      case Right(result) => (either.state, result._2._1._2)
    })
  } yield ParseResult(result3._1, IfExpression(result1.result._1._2, result2.result._1._2, result3._2))
    

  def parse_boolean_expression(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    for {
      result1 <- ParseAction(this.parse_boolean_term).run(scanner);
      result2 <- (ParseAction(this.parse_boolean_operators) guard (ParseAction(this.parse_boolean_expression) orM ParseAction(this.parse_boolean_term))).run(result1.state)
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
    val parseArithExpr = map(ParseAction(this.parse_arith_expression) and
      (GreaterEqualToken orM LessEqualToken orM EqualToken orM NotEqualToken orM GreaterToken orM LessToken) and
      ParseAction(this.parse_arith_expression))(x => {
        OperationCallExpression(x._1._2, x._1._1, x._2)
      })
    
    val parseBooleanConstant = map(BooleanAndToken orM BooleanOrToken)(x => BooleanLiteralExpression(x, x.txt.toBoolean))
    
    val action = orM[Expression, Expression, Expression](parseArithExpr)(ParseAction(this.parse_identifier_expression))

    action.run(scanner)
  }

  def parse_boolean_operators(scanner: Scanner) = 
    (BooleanAndToken orM BooleanOrToken).run(scanner)

  def parse_arith_expression(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val operators = (PlusToken orM MinusToken)
    val action = ParseAction(this.parse_arith_term) and (operators guard ParseAction(this.parse_arith_expression))
    action.map(x => {
      x._2 match {
        case Left(()) => x._1
        case Right((op, expr2)) => OperationCallExpression(op, x._1, expr2)
      }
    }).run(scanner)
  }

  def parse_arith_term(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val operators = (ArithMultToken orM ArithDivideToken orM ModToken)
    val action = ParseAction(this.parse_arith_factor) and (operators guard ParseAction(this.parse_arith_term))
    action.map(x => {
      x._2 match {
        case Left(()) => x._1
        case Right((op, expr2)) => OperationCallExpression(op, x._1, expr2)
      }
    }).run(scanner)
  }

  def parse_arith_factor(scanner: Scanner): Either[List[ParseError], ParseResult[Expression]] = {
    val numberExpr = ParseAction(this.parse_number_literal_expression)
    val booleanExr = ParseAction(this.parse_boolean_operators)
    val identifierOrFuncallCall = map(ParseAction(this.parse_identifier_expression) and (LParanToken guard ParseAction(this.parse_function_call_args) and RParanToken))(x => {
      x._2._1 match {
        case Left(()) => x._1
        case Right(right) => FunctionCallExpression(FunctionDeclaration(right._1, null, null), right._2: _*)
      }
    })
    val arithExprWrapByParan = LParanToken guard ParseAction(this.parse_arith_expression) and RParanToken

    // (arithExprWrapByParan orM identifierOrFuncallCall orM booleanExr orM numberExpr).run(scanner)
    identifierOrFuncallCall.run(scanner)
  }

  def parse_number_literal_expression(scanner: Scanner): Either[List[ParseError], ParseResult[NumberLiteralExpression[_]]] = {
    expectTokenType(NumberToken, x => {
      if (x.txt.contains(".")) {
        FloatLiteralExpression(x, x.txt.toFloat)
      } else {
        FloatLiteralExpression(x, x.txt.toInt)
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
    val action = DefToken and ParseAction(this.parse_identifier_expression) and LParanToken and ParseAction(this.parse_parameter_list) and RParanToken
    action.map(x => {
      // ((((Token, IdentifierExpression), Token), List[Parameter]), Token)
      FunctionDeclaration(x._1._1._1._2.token, x._1._2, null)
    }).run(scanner)
  }

  def parse_parameter_list(scanner: Scanner): Either[List[ParseError], ParseResult[List[Parameter]]] = {
    val action = ParseAction(this.parse_parameter) guard (many(CommaToken and ParseAction(this.parse_parameter)))
    action.flatMap(value => ParseAction(s => {
      if (value.isLeft) {
        Right(ParseResult(s, Nil))
      } else {
        val result = value.right.get
        Right(ParseResult(s, result._1 :: (result._2.map(_._2))))
      }
    })).run(scanner)
  }
  def parse_parameter(scanner: Scanner): Either[List[ParseError], ParseResult[Parameter]] = {
    val action = ParseAction(this.parse_identifier_expression) and ColumnToken and ParseAction(this.parse_type)
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
    (ParseAction(this.parse_identifier_expression) map (mapping)).run(scanner)
  }
}