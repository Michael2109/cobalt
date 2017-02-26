/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Michael Haywood
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

class TokenizerTest(var str: String) {

  private val tokenDatas: ListBuffer[TokenDataTest] = ListBuffer[TokenDataTest](
    new TokenDataTest("^([<-])".r, TokenTypeTest.RETURN_TYPE),
    new TokenDataTest("^((-)?[0-9]+)".r, TokenTypeTest.INTEGER_LITERAL),
    new TokenDataTest("^((-)?[0-9]+[.][0-9])".r, TokenTypeTest.DOUBLE_LITERAL),
    new TokenDataTest("^([+][=])".r, TokenTypeTest.ADD_OPERATOR),
    new TokenDataTest("^([-][=])".r, TokenTypeTest.SUBTRACT_OPERATOR),
    new TokenDataTest("^([*][=])".r, TokenTypeTest.MULTIPLY_OPERATOR),
    new TokenDataTest("^([/][=])".r, TokenTypeTest.DIVIDE_OPERATOR),
    new TokenDataTest("^(\".*\")".r, TokenTypeTest.STRING_LITERAL),
    new TokenDataTest("^([;])".r, TokenTypeTest.END_STATEMENT),
    new TokenDataTest("^([:])".r, TokenTypeTest.COLON),
    new TokenDataTest("^([==])".r, TokenTypeTest.EQUAL_TO),
    new TokenDataTest("^([<])".r, TokenTypeTest.SMALLER_THAN),
    new TokenDataTest("^([<=])".r, TokenTypeTest.SMALLER_THAN_EQUAL),
    new TokenDataTest("^([>])".r, TokenTypeTest.LARGER_THAN),
    new TokenDataTest("^([>=])".r, TokenTypeTest.LARGER_THAN_EQUAL),
    new TokenDataTest("^([a-zA-Z][a-zA-Z0-9]*)".r, TokenTypeTest.IDENTIFIER)
  )

  for (t <- List[String]("\\=", "\\(", "\\)", "\\.", "\\,", "\\'"))
    tokenDatas += new TokenDataTest(("^(" + t + ")").r, TokenTypeTest.TOKEN)

  def nextToken: TokenTest = {
    str = str.trim

    if (str.isEmpty) {
      new TokenTest("", TokenTypeTest.EMPTY)
    } else {
      for (data <- tokenDatas) {

        val matched = data.pattern.findFirstIn(str)
        if (matched.isDefined) {
          val token: String = matched.getOrElse("")
          str = str.replace(token, "")
          if (data.getType eq TokenTypeTest.STRING_LITERAL) {
            return new TokenTest(token.substring(1, token.length - 1), TokenTypeTest.STRING_LITERAL)
          }
          else {
            return new TokenTest(token, data.getType)
          }
        }
      }
      throw new IllegalStateException("Could not parse:" + str)
    }
  }

  def hasNextToken: Boolean = !str.isEmpty
}
