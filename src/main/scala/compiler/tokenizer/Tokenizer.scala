/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Cobalt
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package compiler.tokenizer


import scala.collection.mutable.ListBuffer

class Tokenizer(var str: String) {

  private val tokenDatas: ListBuffer[TokenData] = ListBuffer[TokenData](
    new TokenData("^([<-])".r, TokenType.RETURN_TYPE),

    new TokenData("^((-)?[0-9]+(([.][0-9](f|F))|([.](f|F))|(f|F)))".r, TokenType.FLOAT_LITERAL),
    new TokenData("^((-)?[0-9]+(([.][0-9](d|D))|([.](d|D))|(d|D)|([.])|([.][0-9])))".r, TokenType.DOUBLE_LITERAL),

    new TokenData("^((-)?[0-9]+(s|S))".r, TokenType.SHORT_LITERAL),
    new TokenData("^((-)?[0-9]+(l|L))".r, TokenType.LONG_LITERAL),
    new TokenData("^((-)?[0-9]+)".r, TokenType.INTEGER_LITERAL),

    new TokenData("^(\".*\")".r, TokenType.STRING_LITERAL),
    new TokenData("^(\'.\')".r, TokenType.CHARACTER_LITERAL),

    new TokenData("^([+])".r, TokenType.ADD_OPERATOR),
    new TokenData("^([-])".r, TokenType.SUBTRACT_OPERATOR),
    new TokenData("^([*])".r, TokenType.MULTIPLY_OPERATOR),
    new TokenData("^([/])".r, TokenType.DIVIDE_OPERATOR),
    new TokenData("^([%])".r, TokenType.MODULUS_OPERATOR),


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
          if (data.getType == TokenType.STRING_LITERAL) {
            return new Token(token.substring(1, token.length - 1), TokenType.STRING_LITERAL)
          }
          else if (data.getType == TokenType.CHARACTER_LITERAL) {
            return new Token(token.substring(1, token.length - 1), TokenType.CHARACTER_LITERAL)

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
