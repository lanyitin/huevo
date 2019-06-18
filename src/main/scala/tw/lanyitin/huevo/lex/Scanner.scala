package tw.lanyitin.huevo.lex

import tw.lanyitin.common.ast.TokenType._
import tw.lanyitin.common.ast.{Token, TokenType, NullToken}
import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object typeDef {
  type Tokenizer = ListMap[TokenType, Pattern]
}

case class ScannerState(position: Integer, col: Integer, line: Integer)

abstract class Scanner(val content: String, val state: ScannerState) {
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

  private def _nextToken: Option[Token] = {
    tokenizer.collectFirst(new PartialFunction[(TokenType, Pattern), Token] {
      def isDefinedAt(x: (TokenType.TokenType, Pattern)): Boolean = x._2.matcher(content).find()
      def apply(v1: (TokenType.TokenType, Pattern)): Token = {
        Token(v1._1, v1._2.matcher(content).group(0), state.line, state.col)
      }
    })
  }

  def nextToken: (Token, Scanner) = {
    if (state.position >= content.size) {
      (new Token(EOFToken, "<EOF>", state.line, state.col), this)
    } else {
      val option: Option[Token] = this._nextToken
      option match {
        case None => (new Token(UnexpectedToken, "<UnexpectedToken>"), this)
        case Some(token) => this.nextState(token)
      }
    }
  }
  def nextState(token: Token): (Token, Scanner)
  def tokenizer: Tokenizer
  
}

sealed case class ScannerBuilder(val initTokenizer: ListMap[TokenType, Pattern]) {
  def normalMode(content: String, state: ScannerState): Scanner = new Scanner(content, state) {
    import typeDef._
    def tokenizer = initTokenizer.filter(p => p._1 != CommentBodyToken)

    def nextState(result: Token): (Token, Scanner) = {
      result.tokenType match {
        case EqualToken|GreaterEqualToken|LessEqualToken|GreaterToken|LessToken|
             NotEqualToken|BooleanAndToken|BooleanOrToken|
             ArithDivideToken|ArithMultToken|PlusToken|MinusToken|
             IdentifierToken|NumberToken|BooleanConstantToken|StringToken|LParanToken|RParanToken|
             LCurlyBracket|RCurlyBracket|ColumnToken|CommaToken|DefToken|IfToken|ElseToken|
             AssignToken|LetToken|ModToken
         => (result, normalMode(content, state.copy(position=state.position + result.txt.size,col=state.col+result.txt.size)))
        case SpaceToken => normalMode(content, state.copy(position=state.position + result.txt.size,col=state.col+result.txt.size)).nextToken
        case NewLineToken => normalMode(content, state.copy(position=state.position + result.txt.size, col=0, line=state.line+1)).nextToken
        case CommentHeadToken => commentMode(content, state.copy(position=state.position + result.txt.size,col=state.col+result.txt.size)).nextToken
        case EOFToken | UnexpectedToken => (result, this)
      }
    }
  }
  def commentMode(content: String, state: ScannerState): Scanner = new Scanner(content, state) {
    import typeDef._
    def tokenizer: Tokenizer = initTokenizer.filter(p => p._1 == CommentBodyToken)
    def nextState(token: Token): (Token, Scanner) = {
      (token, normalMode(content, state.copy(position=state.position + token.txt.size,col=state.col+token.txt.size)))
    }
  }
  // def stringMode()
}


object Scanner {
  def apply(txt: String): Scanner = {
    ScannerBuilder(ListMap(
      LetToken -> Pattern.compile("^(let)"),
      DefToken -> Pattern.compile("^(def)"),
      IfToken -> Pattern.compile("^(if)"),
      ElseToken -> Pattern.compile("^(else)"),
      BooleanConstantToken -> Pattern.compile("^(true|false)"),   
      NewLineToken -> Pattern.compile("^(\\r\\n|\\n|\\r)"),
      SpaceToken -> Pattern.compile("^([\\t ]+)"),
      NumberToken -> Pattern.compile("^(\\d+(\\.\\d+)?)"),
      CommaToken -> Pattern.compile("^(,)"),
      ColumnToken -> Pattern.compile("^(:)"),
      EqualToken -> Pattern.compile("^(==)"),
      GreaterEqualToken -> Pattern.compile("^(>=)"),
      LessEqualToken -> Pattern.compile("^(<=)"),
      NotEqualToken -> Pattern.compile("^(!=)"),    
      GreaterToken -> Pattern.compile("^(>)"),
      LessToken -> Pattern.compile("^(<)"),        
      BooleanAndToken -> Pattern.compile("^(and)"),
      BooleanOrToken -> Pattern.compile("^(or)"),
      AssignToken -> Pattern.compile("^(=)"),
      ArithMultToken -> Pattern.compile("^(\\*)"),
      ArithDivideToken -> Pattern.compile("^(/)"),
      PlusToken -> Pattern.compile("^(\\+)"),
      ModToken -> Pattern.compile("^(%)"),
      MinusToken -> Pattern.compile("^(-)"),
      LCurlyBracket -> Pattern.compile("^(\\{)"),
      RCurlyBracket -> Pattern.compile("^(\\})"),
      LParanToken -> Pattern.compile("^(\\()"),
      RParanToken -> Pattern.compile("^(\\))"),
      CommentHeadToken -> Pattern.compile("^(#)"),
      IdentifierToken -> Pattern.compile("^([a-zA-Z][a-zA-Z0-9]*)"),
      CommentBodyToken -> Pattern.compile(".*[^\\r\\n|\\n|\\r]")
    )).normalMode(txt, ScannerState(0, 0, 0))
  }
}