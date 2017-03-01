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

package compiler.structure.parsers.prints

import compiler.structure.blocks.Block
import compiler.structure.blocks.prints.PrintBlock
import compiler.structure.parsers.Parser
import compiler.tokenizer.Tokenizer

// todo set up print parser
class PrintParser extends Parser[PrintBlock] {
  var printVariable: Boolean = false

  /**
    * A list of all regular expressions
    *
    * @return
    */
  override def getRegexs: List[String] = List()

  override def shouldParse(line: String): Boolean = {

    if (true)
      return false
    // Decide whether printing a variable or a string
    if (line.matches("print[ ]*\\([\"].*[\"]\\)")) {
      printVariable = false
      return true
    }
    else if (line.matches("print[ ]*\\(.*\\)")) {
      printVariable = true
      return true
    }
    return false
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): PrintBlock = {
    tokenizer.nextToken // skip print
    tokenizer.nextToken // skip (
    val value: String = tokenizer.nextToken.token
    return new PrintBlock(superBlock, value, printVariable)
  }

}
