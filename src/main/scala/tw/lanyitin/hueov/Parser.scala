package tw.lanyitin.huevo

import TokenType._

object MatcherGenerator {
  type TokenMatcher = Scanner => (Boolean, Scanner, List[Token])
  def byType(tokenType: TokenType): TokenMatcher = {
    (scanner) => {
      val (token, next) = scanner.nextToken()
      if (token.tokenType == tokenType) {
        (true, next, List(token))
      } else {
        (false, scanner, Nil)
      }
    }
  }

  def byText(txt: String): TokenMatcher = {
    (scanner) => {
      val (token, next) = scanner.nextToken()
      if (token.txt == txt) {
        (true, next, List(token))
      } else {
        (false, scanner, Nil)
      }
    }
  }

  def and(m1: TokenMatcher, m2: TokenMatcher): TokenMatcher = {
    (scanner) => {
      val (r1, next, token) = m1(scanner)
      val (r2, _, _) = m2(scanner)
      if (r1 && r2) {
        (true, next, token)
      } else {
        (false, scanner, Nil)
      }
    }
  }

  def or(m1: TokenMatcher, m2: TokenMatcher): TokenMatcher = {
    (scanner) => {
      val (r1, next, token) = m1(scanner)
      if (r1) {
        (true, next, token)
      } else {
        val (r2, next2, token2) = m2(scanner)
        if (r2) {
          (true, next2, token2)
        } else {
          (false, scanner, Nil)
        }
      }
    }
  }

  def epsilon: TokenMatcher = {
    (scanner) => (true, scanner, Nil)
  }
}

case class Node(token: Token, childs: List[Node]) {
  override def toString: String = {
    if (!childs.isEmpty) {
      s"Node($token,$childs)"
    } else {
      s"Node($token)"
    }
  }
}

object Parser {
  import MatcherGenerator._
  def expect(scanner: Scanner, matchers: TokenMatcher*): Option[(Scanner, List[Token])] = {
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

  def parse(scanner: Scanner) = {
    val scanner2 = scanner.skip( (token) => {
      token.tokenType == SpaceToken || token.tokenType == NewLineToken;
    } )
    val next_token = scanner2.take(1)(0)
    if (next_token.tokenType == DefToken) {
      parse_function_defintion(scanner2)
    } else {
      parse_arith_expression(scanner2)
    }
  }

  def parse_function_defintion(scanner: Scanner): (Node, Scanner) = {
    var (fun_decl, scanner2) = parse_function_declaration(scanner)
    val matchers = and(byType(OperatorToken), byText("=")) :: byType(LCurlyBracket) :: or(byType(NewLineToken), epsilon) :: Nil
    val exp_result = expect(scanner2, matchers:_*)
    if (exp_result.isEmpty) {
      throw new Exception()
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
      throw new Exception()
    }
    val (fun_type, scanner7) = parse_type(expectResult.get._1)
    (Node(id_token, List(arg_list, fun_type)), scanner7)
  }

  def parse_type(scanner: Scanner): (Node, Scanner) = {
    val (token, next_state) = scanner.nextToken()
    if (token.tokenType == IdentifierToken) {
      (Node(token, Nil), next_state)
    } else {
      throw new Exception()
    }
  }

  def parse_argument_list(scanner: Scanner): (Node, Scanner) = {
    def parse_rest_arg_list(scanner: Scanner, acc: List[Node]): (List[Node], Scanner) = {
      val matcher = byType(CommaToken) :: byType(IdentifierToken) :: byType(ColumnToken) :: Nil
      val expectResult = expect(scanner, matcher:_*)
      if (!expectResult.isEmpty) {
        val (type_node, scanner2) = parse_type(expectResult.get._1)
        parse_rest_arg_list(scanner2, Node(expectResult.get._2(1), List(type_node)) :: acc)
      } else {
        (acc.reverse, scanner)
      }   
    }

    val argumentMatchMatcher = byType(IdentifierToken) :: byType(ColumnToken) :: Nil
    val expectResult = expect(scanner, argumentMatchMatcher:_*)
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

  def gen_graphviz(node: Node): String = {
    val child_strs = node.childs.flatMap(x => f"${node.token.id} -> ${x.token.id}" :: gen_graphviz(x) :: Nil)
    val node_label = node.token.id+ " [label=\"" + node.token.txt + "\"]"
    (node_label :: child_strs).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val content = "1 + 2 * 3 / 4 * (1 + 2) / (2 * 4) + ((1 * 3 / 3 - 1))"
    val scanner = Scanner(content, (x:String) => ())
    val (tree, _) = Parser.parse(scanner)
    println(gen_graphviz(tree))
  }
}
