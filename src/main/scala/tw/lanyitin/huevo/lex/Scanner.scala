package tw.lanyitin.huevo.lex

import java.util.regex.Pattern
import scala.annotation.tailrec

object typeDef {
  type Tokenizer = Tuple2[Pattern, TokenFn]
  type TokenFn = (String, Integer, Integer) => Token
}

case class ScannerState(position: Integer, col: Integer, line: Integer)

abstract class Scanner(val content: String, val state: ScannerState) {
  import TokenType._
  import typeDef._
  private[this] def _loop(scanner: Scanner, acc: List[Token], p: (Token) => Boolean): (Scanner, List[Token]) = {
    val (token, next_state) = scanner.nextToken
    if (!p(token) || token.tokenType == EOFToken) {
      (scanner, (token :: acc).reverse)
    } else {
      _loop(next_state, token :: acc, p)
    }
  }

  def takeWhile(p: (Token) => Boolean): List[Token] = {
    _loop(this, Nil, p)._2
  }


  def skip(p: (Token) => Boolean): Scanner = {
    _loop(this, Nil, p)._1
  }

  def skip(num: Integer): Scanner = {
    _loop(this, Nil, {
      var n: Integer = num + 1
      (t: Token) => {
        n = n - 1
        n > 0
      }
    })._1
  }

  def take(num: Int): List[Token] = {
    var n = num
    val fn = (_: Token) => {n-=1;n > 0}
    takeWhile(fn)
  }
  def tokenizers: List[Tokenizer]
  def nextToken: (Token, Scanner)
}

case class CommentModeScanner(override val content: String, override val state: ScannerState) extends Scanner(content, state) {
  import typeDef._
  import TokenType._
  def tokenizers: List[Tuple2[Pattern, TokenFn]] =
    (Pattern.compile(".*[^\\r\\n|\\n|\\r]"), (txt: String, line: Integer, col: Integer) => new Token(CommentBodyToken, txt, line, col)) ::
    Nil
  def nextToken: (Token, Scanner) = {
    val matcher = tokenizers.head._1.matcher(content)
    if (matcher.find()) {
      val token = tokenizers.head._2(matcher.group(0), state.line, state.col)
      (token, NormalModeScanner(content, state.copy(position=state.position + token.txt.size,col=state.col+token.txt.size)))
    } else {
      (new Token(UnexpectedToken, "<UnexpectedToken>"), this)
    }
  }
}

case class NormalModeScanner(override val content: String, override val state: ScannerState) extends Scanner(content, state) {
  import TokenType._
  import typeDef._

  def tokenizers: List[Tuple2[Pattern, TokenFn]] =
    (Pattern.compile("^(let)"), (txt: String, line: Integer, col: Integer) => new Token(LetToken, txt, line, col)) ::
    (Pattern.compile("^(def)"), (txt: String, line: Integer, col: Integer) => new Token(DefToken, txt, line, col)) ::
    (Pattern.compile("^(if)"), (txt: String, line: Integer, col: Integer) => new Token(IfToken, txt, line, col)) ::
    (Pattern.compile("^(else)"), (txt: String, line: Integer, col: Integer) => new Token(ElseToken, txt, line, col)) ::
    (Pattern.compile("^(true|false)"), (txt: String, line: Integer, col: Integer) => new Token(BooleanConstantToken, txt, line, col)) ::   
    (Pattern.compile("^(\\r\\n|\\n|\\r)"), (txt: String, line: Integer, col: Integer) => new Token(NewLineToken, txt, line, col)) ::
    (Pattern.compile("^([\\t ]+)"), (txt: String, line: Integer, col: Integer) => new Token(SpaceToken, txt, line, col)) ::
    (Pattern.compile("^(\\d+(\\.\\d+)?)"), (txt: String, line: Integer, col: Integer) => new Token(NumberToken, txt, line, col)) ::
    (Pattern.compile("^(,)"), (txt: String, line: Integer, col: Integer) => new Token(CommaToken, txt, line, col)) ::
    (Pattern.compile("^(:)"), (txt: String, line: Integer, col: Integer) => new Token(ColumnToken, txt, line, col)) ::
    (Pattern.compile("^(==)"), (txt: String, line: Integer, col: Integer) => new Token(EqualToken, txt, line, col)) ::
    (Pattern.compile("^(>=)"), (txt: String, line: Integer, col: Integer) => new Token(GreaterEqualToken, txt, line, col)) ::
    (Pattern.compile("^(>=)"), (txt: String, line: Integer, col: Integer) => new Token(LessEqualToken, txt, line, col)) ::
    (Pattern.compile("^(!=)"), (txt: String, line: Integer, col: Integer) => new Token(NotEqualToken, txt, line, col)) ::    
    (Pattern.compile("^(>)"), (txt: String, line: Integer, col: Integer) => new Token(GreaterToken, txt, line, col)) ::
    (Pattern.compile("^(<)"), (txt: String, line: Integer, col: Integer) => new Token(LessToken, txt, line, col)) ::        
    (Pattern.compile("^(and)"), (txt: String, line: Integer, col: Integer) => new Token(BooleanAndToken, txt, line, col)) ::
    (Pattern.compile("^(or)"), (txt: String, line: Integer, col: Integer) => new Token(BooleanOrToken, txt, line, col)) ::
    (Pattern.compile("^(=)"), (txt: String, line: Integer, col: Integer) => new Token(AssignToken, txt, line, col)) ::
    (Pattern.compile("^(\\*)"), (txt: String, line: Integer, col: Integer) => new Token(ArithMultToken, txt, line, col)) ::
    (Pattern.compile("^(/)"), (txt: String, line: Integer, col: Integer) => new Token(ArithDivideToken, txt, line, col)) ::
    (Pattern.compile("^(\\+)"), (txt: String, line: Integer, col: Integer) => new Token(PlusToken, txt, line, col)) ::
    (Pattern.compile("^(-)"), (txt: String, line: Integer, col: Integer) => new Token(MinusToken, txt, line, col)) ::
    (Pattern.compile("^(\\{)"), (txt: String, line: Integer, col: Integer) => new Token(LCurlyBracket, txt, line, col)) ::
    (Pattern.compile("^(\\})"), (txt: String, line: Integer, col: Integer) => new Token(RCurlyBracket, txt, line, col)) ::
    (Pattern.compile("^(\\()"), (txt: String, line: Integer, col: Integer) => new Token(LParanToken, txt, line, col)) ::
    (Pattern.compile("^(\\))"), (txt: String, line: Integer, col: Integer) => new Token(RParanToken, txt, line, col)) ::
    (Pattern.compile("^(#)"), (txt: String, line: Integer, col: Integer) => new Token(CommentHeadToken, txt, line, col)) ::
    (Pattern.compile("^([a-zA-Z][a-zA-Z0-9]*)"), (txt: String, line: Integer, col: Integer) => new Token(IdentifierToken, txt, line, col)) ::
    Nil
  def nextToken: (Token, Scanner) = {
    // TODO: rewrite loop with built-in operation like collectFirst
    @tailrec
    def loop(tokenizer: List[Tokenizer]): Token = {
      if (tokenizer.isEmpty) {
        new Token(UnexpectedToken, "<UnexpectedToken>")
      } else {
        val matcher = tokenizer.head._1.matcher(content.substring(state.position))
        if (matcher.find()) {
          tokenizer.head._2(matcher.group(0), state.line, state.col)
        } else {
          loop(tokenizer.tail)
        }
      }
    }
    
    if (state.position >= content.size) {
      (new Token(EOFToken, "<EOF>", state.line, state.col), this)
    } else {
      val result = loop(tokenizers)
      result.tokenType match {
        case EqualToken|GreaterEqualToken|LessEqualToken|GreaterToken|LessToken|
             NotEqualToken|BooleanAndToken|BooleanOrToken|
             ArithDivideToken|ArithMultToken|PlusToken|MinusToken|
             IdentifierToken|NumberToken|BooleanConstantToken|StringToken|LParanToken|RParanToken|
             LCurlyBracket|RCurlyBracket|ColumnToken|CommaToken|DefToken|IfToken|ElseToken|
             AssignToken|LetToken
         => (result, NormalModeScanner(content, state.copy(position=state.position + result.txt.size,col=state.col+result.txt.size)))
        case SpaceToken => NormalModeScanner(content, state.copy(position=state.position + result.txt.size,col=state.col+result.txt.size)).nextToken
        case NewLineToken => NormalModeScanner(content, state.copy(position=state.position + result.txt.size, col=0, line=state.line+1)).nextToken
        case CommentHeadToken => CommentModeScanner(content, state.copy(position=state.position + result.txt.size,col=state.col+result.txt.size)).nextToken
        case EOFToken | UnexpectedToken => (result, this)
      }
    }
  }
}


object Scanner {
  // keep tracking of how many token be scanned
  var tokenId = 0

  def apply(txt: String): Scanner = {
    NormalModeScanner(txt, ScannerState(0, 0, 0))
  }
}