package compiler.tokenizer


import scala.collection.mutable.ListBuffer

class Tokenizer(var str: String) {

  private val tokenDatas: ListBuffer[TokenData] = ListBuffer[TokenData](
    new TokenData("^([<-])".r, TokenType.RETURN_TYPE),
    new TokenData("^((-)?[0-9]+)".r, TokenType.INTEGER_LITERAL),
    new TokenData("^((-)?[0-9]+[.][0-9])".r, TokenType.DOUBLE_LITERAL),
    new TokenData("^([+][=])".r, TokenType.ADD_OPERATOR),
    new TokenData("^([-][=])".r, TokenType.SUBTRACT_OPERATOR),
    new TokenData("^([*][=])".r, TokenType.MULTIPLY_OPERATOR),
    new TokenData("^([/][=])".r, TokenType.DIVIDE_OPERATOR),
    new TokenData("^(\".*\")".r, TokenType.STRING_LITERAL),
    new TokenData("^([;])".r, TokenType.END_STATEMENT),
    new TokenData("^([:])".r, TokenType.COLON),
    new TokenData("^([==])".r, TokenType.EQUAL_TO),
    new TokenData("^([<])".r, TokenType.SMALLER_THAN),
    new TokenData("^([<=])".r, TokenType.SMALLER_THAN_EQUAL),
    new TokenData("^([>])".r, TokenType.LARGER_THAN),
    new TokenData("^([>=])".r, TokenType.LARGER_THAN_EQUAL),
    new TokenData("^([a-zA-Z][a-zA-Z0-9]*)".r, TokenType.IDENTIFIER)
  )

  for (t <- List[String]("\\=", "\\(", "\\)", "\\.", "\\,", "\\'"))
    tokenDatas += new TokenData(("^(" + t + ")").r, TokenType.TOKEN)

  def nextToken: Token = {
    str = str.trim

    if (str.isEmpty) {
      new Token("", TokenType.EMPTY)
    } else {
      for (data <- tokenDatas) {

        val matched = data.pattern.findFirstIn(str)
        if (matched.isDefined) {
          val token: String = matched.getOrElse("")
          str = str.replace(token, "")
          if (data.getType eq TokenType.STRING_LITERAL) {
            return new Token(token.substring(1, token.length - 1), TokenType.STRING_LITERAL)
          }
          else {
            return new Token(token, data.getType)
          }
        }
      }
      throw new IllegalStateException("Could not parse:" + str)
    }
  }

  def hasNextToken: Boolean = !str.isEmpty
}
