package tw.lanyitin.huevo

import TokenType._
import ast.Node
import scala.annotation.tailrec

object Parser {
  import MatcherGenerator._

  def parse(scanner: Scanner, acc: List[Node] = Nil): (Node, Scanner) = {
    var acc: List[Node] = Nil
    var final_state: Scanner = scanner

      try {
        while(true) {
          var expr_option = parse_expression(final_state)
          acc = expr_option._1 :: acc
          final_state = expr_option._2
        }
      } catch {
        case e: Exception => {

        }
      }
    if (acc.isEmpty) {
      (Node(new Token(NotExistToken, ""), Nil), scanner)
    } else if (acc.size == 1) {
      (acc(0), final_state)
    } else {
      (Node(new Token(NotExistToken, ""), acc.reverse), final_state)
    }
  }

  def parse_expression(scanner: Scanner): (Node, Scanner) = {
    val scanner2 = scanner.skip( (token) => {
      token.tokenType == SpaceToken || token.tokenType == NewLineToken;
    } )
    val next_token = scanner2.take(1)(0)
    if (next_token.tokenType == DefToken) {
      parse_function_defintion(scanner2)
    } else if (next_token.tokenType == IdentifierToken || next_token.tokenType == NumberToken){
      parse_arith_expression(scanner2)
    } else if (next_token.tokenType == IfToken) {
      parse_if_expression(scanner2)
    } else {
      throw new Exception(next_token.toString)
    }
  }

  def parse_if_expression(scanner: Scanner): (Node, Scanner) = {
    var exp_result = expect(scanner, byType(IfToken), byType(LParanToken))
    if (exp_result.isEmpty) {
      throw new Exception(scanner.take(2).toString)
    }
    val first_token = exp_result.get._2(0)
    val (condition, scanner2) = parse_boolean_expression(exp_result.get._1)
    exp_result = expect(scanner2, byType(RParanToken), byType(LCurlyBracket), or(byType(NewLineToken), epsilon))
    if (exp_result.isEmpty) {
      throw new Exception(scanner2.take(2).toString)
    }
    val (true_path, scanner3) = parse(exp_result.get._1)
    exp_result = expect(scanner3, byType(RCurlyBracket), byType(ElseToken))
    if (exp_result.isEmpty) {
      throw new Exception(scanner2.take(2).toString)
    }
    val (else_path, scanner4) = if (exp_result.get._1.nextToken()._1.tokenType == IfToken) {
      parse_if_expression(exp_result.get._1)
    } else {
      exp_result = expect(exp_result.get._1, byType(LCurlyBracket))
      if (exp_result.isEmpty) {
        throw new Exception(scanner2.take(2).toString)
      }
      val (b, s) = parse(exp_result.get._1)
      exp_result = expect(exp_result.get._1, byType(RCurlyBracket))
      if (exp_result.isEmpty) {
        throw new Exception(scanner2.take(2).toString)
      }
      (b,s)
    }

    (Node(first_token, condition :: true_path :: else_path :: Nil), scanner4)
  }

  def parse_boolean_expression(scanner: Scanner): (Node, Scanner) = ???

  def parse_function_defintion(scanner: Scanner): (Node, Scanner) = {
    var (fun_decl, scanner2) = parse_function_declaration(scanner)
    val exp_result = expect(scanner2, and(byType(OperatorToken), byText("=")), byType(LCurlyBracket), or(byType(NewLineToken), epsilon))
    if (exp_result.isEmpty) {
      throw new Exception(scanner2.take(2).toString)
    }
    val (expressions, scanner3) = parse(exp_result.get._1)
    val exp_result2= expect(scanner3, or(byType(NewLineToken), epsilon), byType(RCurlyBracket))
    if (exp_result2.isEmpty) {
      throw new Exception(scanner3.take(2).toString)
    }
    (Node(new Token(OperatorToken, "="), List(fun_decl, expressions)), exp_result2.get._1)
  }

  def parse_function_declaration(scanner: Scanner): (Node, Scanner) = {
    var expectResult = expect(scanner,byType(DefToken), byType(IdentifierToken), byType(LParanToken))

    if (expectResult.isEmpty) {
      throw new Exception(scanner.take(3).toString)
    }
    val id_token = expectResult.get._2(1)
    var (arg_list, scanner5) = parse_argument_list(expectResult.get._1)
    expectResult = expect(scanner5, byType(RParanToken), byType(ColumnToken))
    if (expectResult.isEmpty) {
      throw new Exception(scanner5.take(2).toString)
    }
    val (fun_type, scanner7) = parse_type(expectResult.get._1)
    (Node(id_token, List(arg_list, fun_type)), scanner7)
  }

  def parse_type(scanner: Scanner): (Node, Scanner) = {
    val (token, next_state) = scanner.nextToken()
    if (token.tokenType == IdentifierToken) {
      (Node(token, Nil), next_state)
    } else {
      throw new Exception(token.tokenType.toString)
    }
  }

  def parse_argument_list(scanner: Scanner): (Node, Scanner) = {
    @tailrec
    def parse_rest_arg_list(scanner: Scanner, acc: List[Node]): (List[Node], Scanner) = {
      val expectResult = expect(scanner, byType(CommaToken), byType(IdentifierToken), byType(ColumnToken))
      if (!expectResult.isEmpty) {
        val (type_node, scanner2) = parse_type(expectResult.get._1)
        parse_rest_arg_list(scanner2, Node(expectResult.get._2(1), List(type_node)) :: acc)
      } else {
        (acc.reverse, scanner)
      }   
    }

    val expectResult = expect(scanner, byType(IdentifierToken), byType(ColumnToken))
    if (!expectResult.isEmpty) {
      val (type_node, scanner2) = parse_type(expectResult.get._1)
      val r = parse_rest_arg_list(scanner2, List(Node(expectResult.get._2(0), List(type_node))))
      (Node(new Token(NotExistToken, ""), r._1), r._2)
    } else {
      throw new Exception(scanner.take(20).toString)
    }
  }

  def parse_arith_expression(scanner: Scanner): (Node, Scanner) = {
    val (arith_term, scanner2) = parse_arith_term(scanner)
    val matcher = and(byType(OperatorToken), or(byText("+"), byText("-")))
    val match_result = matcher(scanner2)
    if (match_result._1) {
        val (arith_exp, scanner4) = parse_arith_expression(match_result._2)
        (Node(match_result._3(0), List(arith_term, arith_exp)), scanner4)
    } else {
      (arith_term, scanner2)
    }
  }

  def parse_arith_term(scanner: Scanner): (Node, Scanner) = {
    val (arith_factor, scanner2) = parse_arith_factor(scanner)
    val matcher = and(byType(OperatorToken), or(byText("*"), byText("/")))
    val match_result = matcher(scanner2)
    if (match_result._1) {
        val (arith_term, scanner4) = parse_arith_term(match_result._2)
        (Node(match_result._3(0), List(arith_factor, arith_term)), scanner4)
    } else {
      (arith_factor, scanner2)
    }
  }

  def parse_arith_factor(scanner: Scanner): (Node, Scanner) = {
    val (token, scanner2) = scanner.nextToken()
    token.tokenType match {
      case NumberToken | IdentifierToken => (Node(token, Nil), scanner2)
      case LParanToken => {
        val (arith_exp, scanner3) = parse_arith_expression(scanner2)
        val (_, scanner4) = scanner3.nextToken()
        (arith_exp, scanner4)
      }
    }
  }


  def expect(scanner: Scanner, matchers: TokenMatcher*): Option[(Scanner, List[Token])] = {
    @tailrec
    def loop(matchers: Seq[TokenMatcher], scanner: Scanner, acc: List[Token]): Option[(Scanner, List[Token])] = {
      if (matchers.isEmpty) {
        Some((scanner, acc))
      } else {
        val (result, next_state, tokens) = matchers.head(scanner)
        if (result) {
          loop(matchers.tail, next_state, acc ::: tokens)
        } else {
          None
        }
      }
    }
    loop(matchers, scanner, Nil)
  }
}
