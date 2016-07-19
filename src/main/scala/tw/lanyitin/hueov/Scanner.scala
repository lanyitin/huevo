package tw.lanyitin.huevo


object TokenType extends Enumeration {
  val Keyword = Value
  val Identifier = Value
  val Number = Value
  val EOF = Value
}

class Token(
  val Text: String,
  val Type: TokenType.Value,
  val LineNum: Int,
  val StartCol: Int,
  val EndCol: Int
)


/** Fundamental Scanner that parse tokens from string
  *
  * @constructor create a new Scanner
  * @param content the text for lexing
  */
class Scanner(val content: String) {
  private val content_length = content.length
  private var index: Int = 0
  private var col: Int = 0
  private var line: Int = 1

// Fetch tokens sequentially
  def NextToken(): Token = {
    if (content(this.index) == ' ' || content(this.index) == '\t') {
      this.index += 1
      this.col += 1
      NextToken()
    } else if (content(this.index) == '\n') {
      this.index += 1
      this.col = 0
      this.line += 1
      NextToken()
    } else if (content(this.index).isLetter) {
      this.parse_keyword_or_identifier()
    } else if (content(this.index).isDigit) {
      this.parse_number()
    } else throw new Exception(s"unexcepted char: ${this.content(this.index)}")
  }

  /** parse keyword or identifier
    *
    */
  private def parse_keyword_or_identifier(): Token = {
    def recursive(buf: String, cur_idx: Int, start_col: Int, cur_col: Int): (Token, Int, Int) = {
      if (cur_idx < this.content_length && this.content(cur_idx).isLetterOrDigit) {
        recursive(buf + this.content(cur_idx), cur_idx + 1, start_col, cur_col + 1)
      } else {
        if (buf == "def") (new Token(buf, TokenType.Keyword, this.line, start_col, cur_col - 1), cur_idx, cur_col)
        else (new Token(buf, TokenType.Identifier, this.line, start_col, cur_col - 1), cur_idx, cur_col)
      }
    }
    val (token, cur_idx, cur_col) = recursive("", this.index, this.col, this.col)
    this.col = cur_col
    this.index = cur_idx
    token
  }
   /** parse number
    *
    */ 
    private def parse_number(): Token = {
    def recursive(buf: String, cur_idx: Int, start_col: Int, cur_col: Int): (Token, Int, Int) = {
      if (cur_idx < this.content_length && this.content(cur_idx).isDigit) {
        recursive(buf + this.content(cur_idx), cur_idx + 1, start_col, cur_col + 1)
      } else  (new Token(buf, TokenType.Number, this.line, start_col, cur_col - 1), cur_idx, cur_col)

    }
    val (token, cur_idx, cur_col) = recursive("", this.index, this.col, this.col)
    this.col = cur_col
    this.index = cur_idx
    token
  }
}
