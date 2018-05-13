    package tw.lanyitin.huevo

import java.util.regex.Pattern

object TokenType extends Enumeration {
  type TokenType = Value
val SpaceToken = Value("SpaceToken") 
val NewLineToken = Value("NewLineToken") 
val EOFToken = Value("EOFToken") 
val UnexpectedToken = Value("UnexpectedToken") 
val NumberToken = Value("NumberToken") 
val StringToken = Value("StringToken") 
val LParanToken = Value("LParanToken") 
val RParanToken = Value("RParanToken") 
val LCurlyBracket = Value("LCurlyBracket") 
val RCurlyBracket = Value("RCurlyBracket") 
val CommaToken = Value("CommaToken") 
val ColumnToken = Value("ColumnToken") 
val DefToken = Value("DefToken") 
val IfToken = Value("IfToken") 
val ElseToken = Value("ElseToken") 
val IdentifierToken = Value("IdentifierToken") 
val OperatorToken = Value("OperatorToken")
val NotExistToken = Value("")
}

sealed class Token(val tokenType: TokenType.TokenType, val txt: String) {
  val id = Scanner.tokenId
  Scanner.tokenId = Scanner.tokenId + 1

  override def toString: String = {
    val espedTxt = txt.replace("\n","\\n").replace("\r", "\\r")
    s"${tokenType}('${espedTxt}')"
  }
}

sealed trait ScannerMode {
  type TokenFn = (String) => Token
  def tokenizer: List[Tuple2[Pattern, TokenFn]]
}

case object NormalMode extends ScannerMode {
  import TokenType._
  def tokenizer: List[Tuple2[Pattern, TokenFn]] =
    (Pattern.compile("^(def)"), (txt: String) => new Token(DefToken, txt)) ::
    (Pattern.compile("^(if)"), (txt: String) => new Token(IfToken, txt)) ::
    (Pattern.compile("^(else)"), (txt: String) => new Token(ElseToken, txt)) ::
    (Pattern.compile("^([a-zA-Z][a-zA-Z0-9]*)"), (txt: String) => new Token(IdentifierToken, txt)) ::
    (Pattern.compile("^(\\r\\n|\\n|\\r)"), (txt: String) => new Token(NewLineToken, txt)) ::
    (Pattern.compile("^([\\t ]+)"), (txt: String) => new Token(SpaceToken, txt)) ::
    (Pattern.compile("^(\\d+)"), (txt: String) => new Token(NumberToken, txt)) ::
    (Pattern.compile("^(,)"), (txt: String) => new Token(CommaToken, txt)) ::
    (Pattern.compile("^(:)"), (txt: String) => new Token(ColumnToken, txt)) ::
    (Pattern.compile("^(\\+|-|\\*|\\/|>|<|=)|and|or|>=|<="), (txt: String) => new Token(OperatorToken, txt)) ::
    (Pattern.compile("^(\\{)"), (txt: String) => new Token(LCurlyBracket, txt)) ::
    (Pattern.compile("^(\\})"), (txt: String) => new Token(RCurlyBracket, txt)) ::
    (Pattern.compile("^(\\()"), (txt: String) => new Token(LParanToken, txt)) ::
    (Pattern.compile("^(\\))"), (txt: String) => new Token(RParanToken, txt)) ::
    Nil
}

case class ScannerState(position: Integer, col: Integer, row: Integer, mode: ScannerMode)

case class Scanner(val content: String, state: ScannerState, print: (String) => Unit) {
  import TokenType._
  def nextToken(): (Token, Scanner) = {
    def loop(tokenizer: List[Tuple2[Pattern, state.mode.TokenFn]]): Token = {
      if (tokenizer.isEmpty) {
        new Token(UnexpectedToken, "<UnexpectedToken>")
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
      (new Token(EOFToken, "<EOF>"), this)
    } else {
      val result = loop(state.mode.tokenizer)
      result.tokenType match {
        case OperatorToken => (result, Scanner(content, state.copy(position=state.position + result.txt.size), print))
        case IdentifierToken => (result, Scanner(content, state.copy(position=state.position + result.txt.size), print))
        case SpaceToken => Scanner(content, state.copy(position=state.position + result.txt.size), print).nextToken()
        case NumberToken => (result, Scanner(content, state.copy(position=state.position + result.txt.size), print))
        case StringToken => (result, Scanner(content, state.copy(position=state.position + result.txt.size), print))
        case NewLineToken => (result, Scanner(content, state.copy(position=state.position + result.txt.size), print))
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

  private[this] def _loop(scanner: Scanner, acc: List[Token], p: (Token) => Boolean): (Scanner, List[Token]) = {
    val (token, next_state) = scanner.nextToken
    if (!p(token) || token.tokenType == EOFToken) {
      (scanner, token :: acc)
    } else {
      _loop(next_state, token :: acc, p)
    }
  }

  def takeWhile(p: (Token) => Boolean): List[Token] = {
    _loop(this, Nil, p)._2.reverse
  }


  def skip(p: (Token) => Boolean): Scanner = {
    _loop(this, Nil, p)._1
  }

  def take(num: Int): List[Token] = {
    var n = num
    val fn = (_: Token) => {n-=1;n > 0}
    takeWhile(fn)
  }
}


object Scanner {
  def apply(txt: String, print: (String) => Unit): Scanner = {
    Scanner(txt, ScannerState(0, 0, 0, NormalMode), print)
  }

  var tokenId = 0
}
