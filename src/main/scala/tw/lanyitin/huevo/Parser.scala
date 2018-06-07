package tw.lanyitin.huevo

import TokenType._
import ast._
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}
import scala.language.implicitConversions

object Parser {
  import MatcherGenerator._

  @tailrec
  def parse(scanner: Scanner, acc: List[Expression] = Nil): Try[(ExprsBlock, Scanner)] = {
    val next_token = scanner.take(1)(0)
    if (next_token.tokenType == EOFToken) {
      Success((ExprsBlock(acc.reverse), scanner))
    } else {
      val result = parse_expression(scanner)
      if (result.isFailure) {
        Failure(result.failed.get)
      } else {
        val (expr: Expression, scanner2: Scanner) = result.get
        parse(scanner2, expr :: acc)
      }
    }
  }

  def parse_expressions_block(scanner: Scanner): Try[(Expression, Scanner)] = {
    @tailrec
    def loop(scanner: Scanner,
             acc: List[Expression] = Nil): Try[(List[Expression], Scanner)] = {
      val next_token = scanner.take(1)(0)
      if (next_token.tokenType == RCurlyBracket) {
        Success((acc.reverse, scanner))
      } else {
        val r = parse_expression(scanner)
        if (r.isFailure) {
          Failure(r.failed.get)
        } else {
          loop(r.get._2, r.get._1 :: acc)
        }
      }
    }

    val r = expect(scanner, LCurlyBracket)
    if (r.isFailure) {
      parse_expression(scanner)
    } else {
      loop(r.get._2).flatMap(r => {
        val (exprs: List[Expression], scanner2: Scanner) = r
        expect(scanner2, RCurlyBracket).flatMap(er => {
          Success((ExprsBlock(r._1), er._2))
        })
      })
    }
  }

  def parse_expression(scanner: Scanner): Try[(Expression, Scanner)] = {
    val next_tokens = scanner.take(2)
    val next_token = next_tokens(0)
    if (next_token.tokenType == DefToken) {
      parse_function_defintion(scanner)
    } else if (next_token.tokenType == NumberToken) {
      parse_arith_expression(scanner)
    } else if (next_token.tokenType == BooleanConstantToken) {
      parse_boolean_expression(scanner)
    } else if (next_token.tokenType == IdentifierToken) {
      (next_tokens(1).tokenType) match {
        case PlusToken | MinusToken | ArithDivideToken | ArithMultToken =>
          parse_arith_expression(scanner)
        case GreaterEqualToken | GreaterToken | LessEqualToken | LessToken |
            EqualToken | NotEqualToken =>
          parse_boolean_expression(scanner)
        case LParanToken =>
          parse_function_call(scanner)
        case CommaToken | RParanToken | RCurlyBracket | EOFToken =>
          val (t, s) = scanner.nextToken
          Success((IdentifierExpression(next_token), s))
      }
    } else if (next_token.tokenType == IfToken) {
      parse_if_expression(scanner)
    } else if (next_token.tokenType == RParanToken) {
      parse_expressions_block(scanner.skip(1))
    } else if (next_token.tokenType == CommentBodyToken) {
      parse_expression(scanner.skip(1))
    } else {
      val current_line = scanner.content.split("\n")(next_token.line)
      Failure(new Exception(
        s"unexpected token ${next_token.tokenType.toString}\n${next_token.line + 1} ${current_line}"))
    }
  }

  def parse_function_call_args(scanner: Scanner): Try[(List[Expression], Scanner)] = {
    @tailrec
    def parse_function_call_args_rest(
        scanner: Scanner,
        acc: List[Expression] = Nil): Try[(List[Expression], Scanner)] = {
      val exp = expect(scanner, RParanToken)
      if (exp.isSuccess) {
        Success((acc.reverse, scanner))
      } else {
        val r = for ((_, scanner2) <- expect(scanner, CommaToken);
                     (expr, scanner3) <- parse_expression(scanner2))
          yield (expr, scanner3)
        if (r.isFailure) {
          Failure(r.failed.get)
        } else {
          parse_function_call_args_rest(r.get._2, r.get._1 :: acc)
        }
      }
    }

    expect(scanner, RParanToken)
      .flatMap(_ => Success((Nil, scanner)))
      .orElse({
        parse_expression(scanner).flatMap(r => {
          val (exp: Expression, scanner2: Scanner) = r
          parse_function_call_args_rest(scanner2, exp :: Nil)
        })
      })
  }

  def parse_function_call(scanner: Scanner): Try[(Expression, Scanner)] = {
    for ((function_name, scanner2) <- expect(
           scanner,
           byType(IdentifierToken) + byType(LParanToken));
         (args, scanner3) <- parse_function_call_args(scanner2);
         (_, scanner4) <- expect(scanner3, RParanToken))
      yield ((new FunctionCallExpression(function_name(0), args:_*), scanner4))
  }

  def parse_if_expression(scanner: Scanner): Try[(Expression, Scanner)] = {
    for ((tokens, scanner2) <- expect(scanner,
                                      byType(IfToken) + byType(LParanToken));
         (condition, scanner3) <- parse_boolean_expression(scanner2);
         (_, scanner4) <- expect(scanner3, RParanToken);
         (true_path, scanner5) <- parse_expressions_block(scanner4);
         (else_token, scanner6) <- expect(scanner5, oneOrZero(ElseToken)))
      yield
        (if (else_token.isEmpty) {
           (IfExpression(condition, true_path), scanner6)
         } else {
           parse_expressions_block(scanner6)
             .map(r => {
               val (false_path: Expression, scanner7: Scanner) = r
               (IfExpression(condition, true_path, false_path),
                scanner7)
             })
             .get
         })
  }

  def parse_boolean_expression(scanner: Scanner): Try[(Expression, Scanner)] = {
    parse_boolean_term(scanner).flatMap(r1 => {
      val (boolean_term: Expression, scanner2: Scanner) = r1
      expect(scanner2, BooleanAndToken or BooleanOrToken)
        .flatMap(r2 => {
          val (tokens: List[Token], scanner3: Scanner) = r2
          parse_boolean_expression(scanner3).flatMap(r3 => {
            val (expr2: Expression, scanner4: Scanner) = r3
            Success((OperationCallExpression(tokens(0), boolean_term, expr2), scanner4))
          })
        })
        .orElse(Success((boolean_term, scanner2)))
    })
  }

  def parse_boolean_term(scanner: Scanner): Try[(Expression, Scanner)] = {
    val result = for (
      (expr1, scanner2) <- parse_arith_expression(scanner);
      (tokens, scanner3) <- expect(scanner2,GreaterToken or LessToken or GreaterEqualToken or LessEqualToken or EqualToken or NotEqualToken);
      (expr2, scanner4) <- parse_arith_expression(scanner3)
    ) yield ((OperationCallExpression(tokens(0), expr1 , expr2), scanner4))
    result.orElse({
        expect(scanner, (IdentifierToken or BooleanConstantToken))
        .flatMap(r1 => {
          val (tokens: List[Token], scanner2: Scanner) = r1
          tokens(0).tokenType match {
            case IdentifierToken => Success((IdentifierExpression(tokens(0)), scanner2))
            case BooleanConstantToken => Success((BooleanLiteralExpression(tokens(0), tokens(0).txt.toBoolean), scanner2))
          }
        })
    })
  }

  def parse_function_defintion(scanner: Scanner): Try[(FunctionDefinitionExpression, Scanner)] = {
    for ((fun_decl, scanner2) <- parse_function_declaration(scanner);
         (tokens, scanner3) <- expect(scanner2, AssignToken);
         (expressions, scanner4) <- parse_expressions_block(scanner3))
      yield (((FunctionDefinitionExpression(fun_decl, expressions), scanner4)))
  }

  def parse_function_declaration(scanner: Scanner): Try[(FunctionDeclaration, Scanner)] = {
    for ((tokens, scanner2) <- expect(
           scanner,
           byType(DefToken) + byType(IdentifierToken) + byType(LParanToken));
         (arg_list, scanner3) <- parse_argument_list(scanner2);
         (_, scanner4) <- expect(scanner3,
                                 byType(RParanToken) + byType(ColumnToken));
         (fun_type, scanner5) <- parse_type(scanner4))
      yield ((FunctionDeclaration(tokens(1), arg_list, fun_type), scanner5))
  }

  def parse_type(scanner: Scanner): Try[(Type, Scanner)] = {
    byType(IdentifierToken)(scanner).flatMap(r => {
      Success((Type(r._1(0)), r._2))
    })
  }

  def parse_argument_list(scanner: Scanner): Try[(List[Parameter], Scanner)] = {
    for ((tokens, scanner2) <- expect(
           scanner,
           byType(IdentifierToken) + byType(ColumnToken));
         (type_node, scanner3) <- parse_type(scanner2))
      yield
        ({
          val (rest_args, scanner4: Scanner) = parse_rest_arg_list(scanner3)
          (Parameter(tokens(0), type_node) :: rest_args, scanner4)
        })
  }

  def parse_rest_arg_list(scanner: Scanner,
                          acc: List[Parameter] = Nil): (List[Parameter], Scanner) = {
    var matcher = byType(CommaToken) + byType(IdentifierToken) + byType(
      ColumnToken)
    expect(scanner, matcher)
      .flatMap(r1 => {
        val (tokens: List[Token], scanner2: Scanner) = r1
        parse_type(scanner2).flatMap(r2 => {
          val (type_node: Type, scanner3: Scanner) = r2
          Success(
            parse_rest_arg_list(scanner3,
                                Parameter(tokens(1), type_node) :: acc))
        })
      })
      .orElse(Success((acc.reverse, scanner)))
      .get
  }

  def parse_arith_expression(scanner: Scanner): Try[(Expression, Scanner)] = {
    parse_arith_term(scanner).flatMap(r1 => {
      val (arith_term: Expression, scanner2: Scanner) = r1
      val matcher = PlusToken or MinusToken
      matcher(scanner2)
        .flatMap(r2 => {
          val (token: List[Token], scanner3: Scanner) = r2
          parse_arith_expression(scanner3).flatMap(r3 => {
            val (arith_exp: Expression, scanner4: Scanner) = r3
            Success((OperationCallExpression(token(0), arith_term, arith_exp), scanner4))
          })
        })
        .orElse(Success((arith_term, scanner2)))
    })
  }

  def parse_arith_term(scanner: Scanner): Try[(Expression, Scanner)] = {
    parse_arith_factor(scanner).flatMap((r1) => {
      val (arith_factor: Expression, scanner2: Scanner) = r1
      val matcher = ArithMultToken or ArithDivideToken
      matcher(scanner2)
        .flatMap((r2) => {
          val (op_token, scanner3) = r2
          parse_arith_term(scanner3).flatMap((r3) => {
            val (arith_term: Expression, scanner4: Scanner) = r3
            Success(
              (OperationCallExpression(op_token(0), arith_factor, arith_term), scanner4))
          })
        })
        .orElse(Success((arith_factor, scanner2)))
    })
  }

  def parse_arith_factor(scanner: Scanner): Try[(Expression, Scanner)] = {
    val (token, scanner2) = scanner.nextToken
    token.tokenType match {
      case NumberToken =>
        Success((NumberLiteralExpression(token, token.txt.toInt), scanner2))
      case IdentifierToken =>
        Success((IdentifierExpression(token), scanner2))
      case LParanToken => {
        for ((arith_exp, scanner3) <- parse_arith_expression(scanner2);
             (_, scanner4) <- expect(scanner3, RParanToken))
          yield ((arith_exp, scanner4))
      }
    }
  }

  def expect(scanner: Scanner,
             matcher: TokenMatcher): Try[(List[Token], Scanner)] = {
    matcher(scanner)
  }
}
