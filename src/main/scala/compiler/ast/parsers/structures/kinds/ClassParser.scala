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

package compiler.ast.parsers.structures.kinds

import compiler.ast.blocks.Block
import compiler.ast.blocks.structures.kinds.ClassBlock
import compiler.ast.parsers.Parser
import compiler.data.parameters.Parameters
import compiler.tokenizer.tokens.EmptyToken
import compiler.tokenizer.tokens.keywords.modifiers.ModifierToken
import compiler.tokenizer.tokens.keywords.{ExtendsToken, ImplementsToken}
import compiler.tokenizer.{TokenType, Tokenizer}

import scala.collection.mutable.ListBuffer

class ClassParser extends Parser[ClassBlock] {

  /**
    * A list of all regular expressions
    *
    * @return list of regexs
    */
  override def getRegexs: List[String] = List(
    "(public[ ]+|protected[ ]+|internal[ ]+)?(open[ ]+|abstract[ ]+)*class[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*\\((([ ]*[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*[a-zA-Z][a-zA-Z0-9]*[ ]*[,]?)*)*\\)([ ]+extends[ ]+[a-zA-Z][a-zA-Z0-9]*)?+([ ]+implements[ ]+[a-zA-Z][a-zA-Z0-9]*([,][a-zA-Z][a-zA-Z0-9]*)*)?:?"
  )

  def parse(superBlock: Block, tokenizer: Tokenizer): ClassBlock = {

    val modifiers: ListBuffer[TokenType] = {
      var result: ListBuffer[TokenType] = ListBuffer()
      while (tokenizer.peek.tokenType.isInstanceOf[ModifierToken]) {
        result += tokenizer.nextToken.tokenType

      }
      result
    }

    println("modifiers:" + modifiers)

    println("class:" + tokenizer.nextToken) // skip "class"

    val className: String = tokenizer.nextToken.token
    println("classname:" + className)
    tokenizer.nextToken
    // skip "("
    var nextToken: String = tokenizer.nextToken.token

    var paramString = ""
    while (nextToken != ")") {
      paramString += nextToken
      nextToken = tokenizer.nextToken.token
    }

    val parameters = new Parameters().getParameters(paramString)

    val extendsTokens: List[TokenType] = List[TokenType](
      if (tokenizer.peek.tokenType.isInstanceOf[ExtendsToken]) {
        tokenizer.nextToken.tokenType
        tokenizer.nextToken.tokenType
      } else {
        new EmptyToken
      }
    ).filter(!_.isInstanceOf[EmptyToken])
    println(extendsTokens)

    val implementsTokens: List[TokenType] = List[TokenType](
      if (tokenizer.peek.tokenType.isInstanceOf[ImplementsToken]) {
        tokenizer.nextToken.tokenType
        tokenizer.nextToken.tokenType
      } else {
        new EmptyToken
      }
    ).filter(!_.isInstanceOf[EmptyToken])

    new ClassBlock(superBlock, modifiers.toList, className, parameters, extendsTokens, implementsTokens)
  }
}
