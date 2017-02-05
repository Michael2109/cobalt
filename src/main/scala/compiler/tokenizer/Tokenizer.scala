package compiler.tokenizer

import java.util.ArrayList
import java.util.regex.Matcher
import java.util.regex.Pattern

class Tokenizer(var str: String) {

  private var tokenDatas: ArrayList[TokenData] = null
  private var lastToken: Token = null
  private var pushBackBool: Boolean = false

  this.tokenDatas = new ArrayList[TokenData]
  tokenDatas.add(new TokenData(Pattern.compile("^([<-])"), TokenType.RETURN_TYPE))
  tokenDatas.add(new TokenData(Pattern.compile("^((-)?[0-9]+)"), TokenType.INTEGER_LITERAL))
  tokenDatas.add(new TokenData(Pattern.compile("^((-)?[0-9]+[.][0-9])"), TokenType.DOUBLE_LITERAL))
  tokenDatas.add(new TokenData(Pattern.compile("^([+][=])"), TokenType.ADD_OPERATOR))
  tokenDatas.add(new TokenData(Pattern.compile("^([-][=])"), TokenType.SUBTRACT_OPERATOR))
  tokenDatas.add(new TokenData(Pattern.compile("^([*][=])"), TokenType.MULTIPLY_OPERATOR))
  tokenDatas.add(new TokenData(Pattern.compile("^([/][=])"), TokenType.DIVIDE_OPERATOR))
  tokenDatas.add(new TokenData(Pattern.compile("^(\".*\")"), TokenType.STRING_LITERAL))
  tokenDatas.add(new TokenData(Pattern.compile("^([;])"), TokenType.END_STATEMENT))
  tokenDatas.add(new TokenData(Pattern.compile("^([:])"), TokenType.COLON))
  tokenDatas.add(new TokenData(Pattern.compile("^([==])"), TokenType.EQUAL_TO))
  tokenDatas.add(new TokenData(Pattern.compile("^([<])"), TokenType.SMALLER_THAN))
  tokenDatas.add(new TokenData(Pattern.compile("^([<=])"), TokenType.SMALLER_THAN_EQUAL))
  tokenDatas.add(new TokenData(Pattern.compile("^([>])"), TokenType.LARGER_THAN))
  tokenDatas.add(new TokenData(Pattern.compile("^([>=])"), TokenType.LARGER_THAN_EQUAL))
  tokenDatas.add(new TokenData(Pattern.compile("^([a-zA-Z][a-zA-Z0-9]*)"), TokenType.IDENTIFIER))

  for (t <- Array[String]("=", "\\(", "\\)", "\\.", "\\,", "\\'")) {
    tokenDatas.add(new TokenData(Pattern.compile("^(" + t + ")"), TokenType.TOKEN))
  }

  def nextToken: Token = {
    str = str.trim
    if (pushBackBool) {
      pushBackBool = false
      return lastToken
    }
    if (str.isEmpty) {
      (lastToken = new Token("", TokenType.EMPTY))
      return lastToken
    }
    import scala.collection.JavaConversions._
    for (data <- tokenDatas) {
      val matcher: Matcher = data.getPattern.matcher(str)
      if (matcher.find) {
        val token: String = matcher.group.trim
        str = matcher.replaceFirst("")
        if (data.getType eq TokenType.STRING_LITERAL) {
          lastToken = new Token(token.substring(1, token.length - 1), TokenType.STRING_LITERAL)
          return lastToken
        }
        else {
          lastToken = new Token(token, data.getType)
          return lastToken
        }
      }
    }
    throw new IllegalStateException("Could not parse " + str)
  }

  def hasNextToken: Boolean = !str.isEmpty

  def pushBack() {
    if (lastToken != null) {
      this.pushBackBool = true
    }
  }
}
