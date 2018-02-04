package tw.lanyitin.huevo

import java.util.regex.Pattern

sealed trait Token
// Misc Tokens
case class SpaceToken(val text: String) extends Token
case class NewLineToken(val text: String) extends Token
case object EOFToken extends Token
case object UnexpectedToken extends Token
// Literal Tokens
case class NumberToken(val text: String) extends Token
case class StringToken(val text: String) extends Token
// Symbol Tokens
case object LParanToken extends Token
case object RParanToken extends Token
case object LCurlyBracket extends Token
case object RCurlyBracket extends Token
case object CommaToken extends Token
case object ColumnToken extends Token
// Keyword Tokens
case object DefToken extends Token
case object IfToken extends Token
case object ElseToken extends Token
// Others
case class IdentifierToken(val text: String) extends Token
case class OperatorToken(val text: String) extends Token

sealed trait ScannerMode {
  type TokenFn = (String) => Token
  def tokenizer: List[Tuple2[Pattern, TokenFn]]
}
case object NormalMode extends ScannerMode {
  def tokenizer: List[Tuple2[Pattern, TokenFn]] =
    (Pattern.compile("^(def)"), (txt: String) => DefToken) ::
    (Pattern.compile("^(if)"), (txt: String) => IfToken) ::
    (Pattern.compile("^(else)"), (txt: String) => ElseToken) ::
    (Pattern.compile("^([a-zA-Z][a-zA-Z0-9]*)"), (txt: String) => IdentifierToken(txt)) ::
    (Pattern.compile("^(\\r|\\r\\n|\\n)"), (txt: String) => NewLineToken(txt)) ::
    (Pattern.compile("^([\\t ]+)"), (txt: String) => SpaceToken(txt)) ::
    (Pattern.compile("^(\\d+)"), (txt: String) => NumberToken(txt)) ::
    (Pattern.compile("^(,)"), (txt: String) => CommaToken) ::
    (Pattern.compile("^(:)"), (txt: String) => ColumnToken) ::
    (Pattern.compile("^(\\+|-|\\*|\\/|>|<|=)|and|or|>=|<="), (txt: String) => OperatorToken(txt)) ::
    (Pattern.compile("^(\\{)"), (txt: String) => LCurlyBracket) ::
    (Pattern.compile("^(\\})"), (txt: String) => RCurlyBracket) ::
    (Pattern.compile("^(\\()"), (txt: String) => LParanToken) ::
    (Pattern.compile("^(\\))"), (txt: String) => RParanToken) ::
    Nil
}

case class ScannerState(position: Integer, col: Integer, row: Integer, mode: ScannerMode)

case class Scanner(val content: String, state: ScannerState, print: (String) => Unit) {
  def nextToken(): (Token, Scanner) = {
    def loop(tokenizer: List[Tuple2[Pattern, state.mode.TokenFn]]): Token = {
      if (tokenizer.isEmpty) {
        UnexpectedToken
      } else {
        val matcher = tokenizer.head._1.matcher(content.substring(state.position))
        if (matcher.find()) {
          tokenizer.head._2(matcher.group(0))
        } else {
          loop(tokenizer.tail)
        }
      }
    }

    if (state.position >= content.size) {
      (EOFToken, this)
    } else {
      val result = loop(state.mode.tokenizer)
      result match {
        case OperatorToken(txt) => (result, Scanner(content, state.copy(position=state.position + txt.size), print))
        case IdentifierToken(txt) => (result, Scanner(content, state.copy(position=state.position + txt.size), print))
        case SpaceToken(txt) => (result, Scanner(content, state.copy(position=state.position + txt.size), print))
        case NumberToken(txt) => (result, Scanner(content, state.copy(position=state.position + txt.size), print))
        case StringToken(txt) => (result, Scanner(content, state.copy(position=state.position + txt.size), print))
        case NewLineToken(txt) => (result, Scanner(content, state.copy(position=state.position + txt.size), print))
        case LParanToken => (result, Scanner(content, state.copy(position=state.position + 1), print))
        case RParanToken => (result, Scanner(content, state.copy(position=state.position + 1), print))
        case LCurlyBracket => (result, Scanner(content, state.copy(position=state.position + 1), print))
        case RCurlyBracket => (result, Scanner(content, state.copy(position=state.position + 1), print))
        case ColumnToken => (result, Scanner(content, state.copy(position=state.position + 1), print))
        case CommaToken => (result, Scanner(content, state.copy(position=state.position + 1), print))
        case DefToken => (result, Scanner(content, state.copy(position=state.position + 3), print))
        case IfToken => (result, Scanner(content, state.copy(position=state.position + 2), print))
        case ElseToken => (result, Scanner(content, state.copy(position=state.position + 4), print))
        case EOFToken => (result, this)
        case UnexpectedToken => (result, this)
      }
    }
  }
  def takeWhile(p: (Token) => Boolean): List[Token] = {
    def loop(scanner: Scanner, acc: List[Token]): List[Token] = {
      val (token, next_state) = scanner.nextToken
      if (!p(token)) {
        token :: acc
      } else {
        loop(next_state, token :: acc)
      }
    }
    loop(this, Nil).reverse
  }
}

object Scanner {
  def apply(txt: String, print: (String) => Unit): Scanner = {
    Scanner(txt, ScannerState(0, 0, 0, NormalMode), print)
  }
}
