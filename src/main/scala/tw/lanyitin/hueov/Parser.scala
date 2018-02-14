package tw.lanyitin.huevo

case class Node(token: Token, childs: List[Node])

object Parser {
  def parse(scanner: Scanner) = {
    parse_arith_expression(scanner)
  }

  def parse_arith_expression(scanner: Scanner): (Node, Scanner) = {
    val (arith_term, scanner2) = parse_arith_term(scanner)
    val (next_token, scanner3) = scanner2.nextToken()
    next_token match {
      case OperatorToken("+") |OperatorToken("-")  => {
        val (arith_exp, scanner4) = parse_arith_expression(scanner3)
        (Node(next_token, List(arith_term, arith_exp)), scanner4)
      }
      case default => (arith_term, scanner2)
    }
  }

  def parse_arith_term(scanner: Scanner): (Node, Scanner) = {
    val (arith_factor, scanner2) = parse_arith_factor(scanner)
    val (next_token, scanner3) = scanner2.nextToken()
    next_token match {
      case OperatorToken("*") |OperatorToken("/")  => {
        val (arith_term, scanner4) = parse_arith_term(scanner3)
        (Node(next_token, List(arith_factor, arith_term)), scanner4)
      }
      case default => (arith_factor, scanner2)
    }
  }

  def parse_arith_factor(scanner: Scanner): (Node, Scanner) = {
    val (token, scanner2) = scanner.nextToken()
    token match {
      case NumberToken(_) | IdentifierToken(_)=> (Node(token, Nil), scanner2)
      case LParanToken() => {
        val (arith_exp, scanner3) = parse_arith_expression(scanner2)
        val (_, scanner4) = scanner3.nextToken()
        (arith_exp, scanner4)
      }
      case default => (Node(token, Nil), scanner2)
    }
  }

  def gen_graphviz(node: Node): String = {
    val child_strs = node.childs.flatMap(x => f"${node.token.id} -> ${x.token.id}" :: gen_graphviz(x) :: Nil)
    val token_txt = node.token match {
        case OperatorToken(txt) => txt
        case IdentifierToken(txt) => txt
        case SpaceToken(txt) => txt
        case NumberToken(txt) => txt
        case StringToken(txt) => txt
        case NewLineToken(txt) => txt
        case LParanToken() => "("
        case RParanToken() => ")"
        case LCurlyBracket() => "{"
        case RCurlyBracket() => "}"
        case ColumnToken() => ":"
        case CommaToken() => ","
        case DefToken() => "def"
        case IfToken() => "if"
        case ElseToken() => "else"
        case EOFToken() => "<EOF>"
        case UnexpectedToken() => "<UnexpectedToken>"
      }
    val node_label = node.token.id+ " [label=\"" + token_txt + "\"]"
    (node_label :: child_strs).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val content = "1 + 2 * 3 / 4 * (1 + 2) / (2 * 4) + ((1 * 3 / 3 - 1))"
    val scanner = Scanner(content, (x:String) => ())
    val (tree, _) = Parser.parse(scanner)
    println(gen_graphviz(tree))
  }
}
