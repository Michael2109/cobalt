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

package cobalt.ast.structures.kinds

import cobalt.ast.{Block, Parser}
import cobalt.data.parameters.Parameters
import cobalt.tokenizer.tokens.EmptyToken
import cobalt.tokenizer.tokens.keywords.modifiers.ModifierToken
import cobalt.tokenizer.tokens.keywords.{ExtendsToken, ImplementsToken}
import cobalt.tokenizer.{Token, Tokenizer}

import scala.collection.mutable.ListBuffer

class ClassParser extends Parser[ClassBlock] {

  /**
    * A list of all regular stack
    *
    * @return list of regexs
    */
  override val regex: String = "(public[ ]+|protected[ ]+|internal[ ]+)?(open[ ]+|abstract[ ]+)*class[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*\\((([ ]*[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*[a-zA-Z][a-zA-Z0-9]*[ ]*[,]?)*)*\\)([ ]+extends[ ]+[a-zA-Z][a-zA-Z0-9]*)?+([ ]+implements[ ]+[a-zA-Z][a-zA-Z0-9]*([,][a-zA-Z][a-zA-Z0-9]*)*)?:?"

  def parse(superBlock: Block, tokenizer: Tokenizer): ClassBlock = {

    val modifiers: ListBuffer[Token] = {
      var result: ListBuffer[Token] = ListBuffer()
      while (tokenizer.peek.tokenType.isInstanceOf[ModifierToken]) {
        result += tokenizer.nextToken
      }
      result
    }

    tokenizer.nextToken // skip "class"

    val className: String = tokenizer.nextToken.token

    tokenizer.nextToken
    // skip "("
    var nextToken: String = tokenizer.nextToken.token

    var paramString = ""
    while (nextToken != ")") {
      paramString += nextToken
      nextToken = tokenizer.nextToken.token
    }

    val parameters = new Parameters().getParameters(paramString)

    val extendsTokens: List[Token] = List[Token](
      if (tokenizer.peek.tokenType.isInstanceOf[ExtendsToken]) {
        tokenizer.nextToken
        tokenizer.nextToken
      } else {
        new Token("", new EmptyToken)
      }
    ).filter(!_.tokenType.isInstanceOf[EmptyToken])

    val implementsTokens: List[Token] = List[Token](
      if (tokenizer.peek.tokenType.isInstanceOf[ImplementsToken]) {
        tokenizer.nextToken
        tokenizer.nextToken
      } else {
        new Token("", new EmptyToken)
      }

    ).filter(!_.tokenType.isInstanceOf[EmptyToken])

    new ClassBlock(superBlock, modifiers.toList, className, parameters, extendsTokens, implementsTokens)
  }
}
