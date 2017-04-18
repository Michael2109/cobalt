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


import compiler.tokenizer.tokens._
import compiler.tokenizer.tokens.constants._
import compiler.tokenizer.tokens.keywords.modifiers._
import compiler.tokenizer.tokens.keywords.{ExtendsToken, ImplementsToken}
import compiler.tokenizer.tokens.operators._

import scala.collection.mutable.ListBuffer

class Tokenizer(var line: String) {

  private val tokenDatas: ListBuffer[TokenType] = ListBuffer[TokenType](

    /* Modifiers */
    new PublicToken,
    new ProtectedToken,
    new InternalToken,
    new AbstractToken,
    new OverrideToken,
    new OpenToken,

    // keywords
    new ExtendsToken,
    new ImplementsToken,

    new ReturnTypeToken,

    new FloatLiteralToken,
    new DoubleLiteralToken,
    new ByteLiteralToken,
    new ShortLiteralToken,
    new LongLiteralToken,
    new IntegerLiteralToken,

    new StringLiteralToken,
    new CharacterLiteralToken,

    new AddOperatorToken,
    new SubtractOperatorToken,
    new MultiplyOperatorToken,
    new DivideOperatorToken,
    new ModulusOperatorToken,

    new EndStatementToken,
    new ColonToken,
    new UnderscoreToken,

    new EqualToToken,
    new SmallerThanToken,
    new SmallerThanEqualToken,
    new LargerThanToken,
    new LargerThanEqualToken,

    new IdentifierToken,

    new AssignmentToken,
    new OpeningBracketToken,
    new ClosingBracketToken,
    new FullStopToken,
    new CommaToken,
    new ApostropheToken
  )

  //line = StringEscapeUtils.escapeJava(line)

  /**
    * Returns the next token without removing it from the line
    *
    * @return The next token
    */
  def peek: Token = {
    line = line.trim

    if (line.isEmpty) {
      new Token("", new EmptyToken)
    } else {
      for (data <- tokenDatas) {
        val matched = data.getRegex().findFirstIn(line)
        if (matched.isDefined && matched.get == line.substring(0, matched.get.size)) {
          val token: String = matched.getOrElse("")
          return new Token(token, data)
        }
      }
      throw new IllegalStateException("Could not parse:" + line)
    }
  }

  /**
    * Returns the next token and removes it from the line
    *
    * @return The next token
    */
  def nextToken: Token = {
    line = line.trim

    if (line.isEmpty) {
      new Token("", new EmptyToken)
    } else {
      for (data <- tokenDatas) {
        val matched = data.getRegex().findFirstIn(line)
        if (matched.isDefined && matched.get == line.substring(0, matched.get.size)) {
          val token: String = matched.getOrElse("")
          line = data.getRegex().replaceFirstIn(line, "")
          return new Token(token, data)
        }
      }
      throw new IllegalStateException("Could not parse:" + line)
    }
  }



  /**
    * Returns whether the next token exists.
    *
    * @return
    */
  def hasNextToken: Boolean = !line.isEmpty
}
