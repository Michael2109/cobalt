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

package compiler.ast.parsers.structures


import compiler.ast.blocks.Block
import compiler.ast.blocks.structures.ObjectDefinitionBlock
import compiler.ast.parsers.Parser
import compiler.tokenizer.Tokenizer
import compiler.utilities.Utils

/**
  * Creation of a new instance of a class
  * E.g. new Object(10,20)
  */
class ObjectDefinitionParser extends Parser[ObjectDefinitionBlock] {


  /**
    * A list of all regular stack
    *
    * @return
    */
  override def getRegexs: List[String] = List(
    "new[ ]*[a-zA-Z][a-zA-Z0-9]*\\(([^]]*)\\)"
  )

  def parse(superBlock: Block, tokenizer: Tokenizer): ObjectDefinitionBlock = {
    val newKeyword: String = tokenizer.nextToken.token
    val initClassName: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip "("

    // Contains everything within the parenthesis
    var argString = ""
    while (tokenizer.peek.token != ")" && tokenizer.peek.token != "") {
      if (tokenizer.peek.token == ",") {
        argString += " "
        tokenizer.nextToken
      }
      else
        argString += tokenizer.nextToken.token.trim
    }

    val argBlocks = Utils.getAllBlocks(superBlock, argString)

    new ObjectDefinitionBlock(superBlock, newKeyword, initClassName, argBlocks)
  }
}
