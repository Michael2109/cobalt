package test_classes.tokenizer

import java.util.ArrayList
import java.util.regex.Matcher
import java.util.regex.Pattern

import scala.collection.mutable.ListBuffer

class Tokenizer(var str: String) {

  private val tokenDatas: ListBuffer[TokenData] = ListBuffer[TokenData](
    new TokenData(Pattern.compile("^([<-])"), TokenType.RETURN_TYPE),
    new TokenData(Pattern.compile("^((-)?[0-9]+)"), TokenType.INTEGER_LITERAL),
    new TokenData(Pattern.compile("^((-)?[0-9]+[.][0-9])"), TokenType.DOUBLE_LITERAL),
    new TokenData(Pattern.compile("^([+][=])"), TokenType.ADD_OPERATOR),
    new TokenData(Pattern.compile("^([-][=])"), TokenType.SUBTRACT_OPERATOR),
    new TokenData(Pattern.compile("^([*][=])"), TokenType.MULTIPLY_OPERATOR),
    new TokenData(Pattern.compile("^([/][=])"), TokenType.DIVIDE_OPERATOR),
    new TokenData(Pattern.compile("^(\".*\")"), TokenType.STRING_LITERAL),
    new TokenData(Pattern.compile("^([;])"), TokenType.END_STATEMENT),
    new TokenData(Pattern.compile("^([:])"), TokenType.COLON),
    new TokenData(Pattern.compile("^([==])"), TokenType.EQUAL_TO),
    new TokenData(Pattern.compile("^([<])"), TokenType.SMALLER_THAN),
    new TokenData(Pattern.compile("^([<=])"), TokenType.SMALLER_THAN_EQUAL),
    new TokenData(Pattern.compile("^([>])"), TokenType.LARGER_THAN),
    new TokenData(Pattern.compile("^([>=])"), TokenType.LARGER_THAN_EQUAL),
    new TokenData(Pattern.compile("^([a-zA-Z][a-zA-Z0-9]*)"), TokenType.IDENTIFIER)
  )

  for (t <- Array[String]("=", "\\(", "\\)", "\\.", "\\,", "\\'")) tokenDatas += new TokenData(Pattern.compile("^(" + t + ")"), TokenType.TOKEN)

  def nextToken: Token = {
    str = str.trim

    if (str.isEmpty) {
      new Token("", TokenType.EMPTY)
    } else {
      for (data <- tokenDatas) {
        val matcher: Matcher = data.getPattern.matcher(str)
        if (matcher.find) {
          val token: String = matcher.group.trim
          str = matcher.replaceFirst("")
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
