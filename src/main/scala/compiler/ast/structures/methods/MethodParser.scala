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

package compiler.ast.structures.methods

import compiler.ast.{Block, Parser}
import compiler.tokenizer.Tokenizer
import compiler.tokenizer.tokens.keywords.modifiers.ModifierToken
import compiler.utilities.Utils

import scala.collection.mutable.ListBuffer

class MethodParser extends Parser[MethodBlock] {

  /**
    * A list of all regular stack
    *
    * @return
    */
  override def getRegexs: List[String] = List(
    "(public[ ]+|protected[ ]+|internal[ ]+)?(open[ ]+|override[ ]+)*[a-zA-Z_][a-zA-Z0-9_]*[ ]*\\((([ ]*[a-zA-Z_][a-zA-Z0-9_]*[ ]*:[ ]*[a-zA-Z_][a-zA-Z0-9_]*[ ]*)*([,]?(([ ]*[a-zA-Z_][a-zA-Z0-9_]*[ ]*:[ ]*[a-zA-Z_][a-zA-Z0-9_]*[ ]*)))*)*\\)([ ]*:[ ]*[a-zA-Z_][a-zA-Z0-9_]*)?:?"
  )

  def parse(superBlock: Block, tokenizer: Tokenizer): MethodBlock = {

    val modifiers: ListBuffer[ModifierToken] = {
      var result: ListBuffer[ModifierToken] = ListBuffer()
      while (tokenizer.peek.tokenType.isInstanceOf[ModifierToken]) {
        result += tokenizer.nextToken.tokenType.asInstanceOf[ModifierToken]
      }
      result
    }

    val isSealed: Boolean = tokenizer.peek.token != "open" // check open

    if (!isSealed)
      tokenizer.nextToken // skip open

    val name: String = tokenizer.nextToken.token // method name

    tokenizer.nextToken // "("
    // Contains everything within the parenthesis
    var paramString = ""
    while (tokenizer.peek.token != ")" && tokenizer.peek.token != "") {
      if (tokenizer.peek.token == ",") {
        paramString += " "
        tokenizer.nextToken
      }
      else
        paramString += tokenizer.nextToken.token.trim
    }

    val paramBlocks = Utils.getAllBlocks(superBlock, paramString)

    tokenizer.nextToken // skip ")"
    tokenizer.nextToken // skip ":"

    val returnType: String = tokenizer.nextToken.token // method return type

    new MethodBlock(superBlock, modifiers.toList, name, returnType, isSealed, paramBlocks)
  }
}
