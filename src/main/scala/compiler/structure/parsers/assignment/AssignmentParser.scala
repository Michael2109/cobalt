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

package compiler.structure.parsers.assignment

import compiler.structure.blocks.Block
import compiler.structure.blocks.assignment.AssignmentBlock
import compiler.structure.parsers.Parser
import compiler.tokenizer.Tokenizer

class AssignmentParser extends Parser[AssignmentBlock] {


  /**
    * A list of all regular expressions
    *
    * @return
    */
  override def getRegexs: List[String] = List(
    //"[a-zA-Z][a-zA-Z0-9]*[ ]*[=][ ]*[.]+[ ]*"
  )

  override def parse(superBlock: Block, tokenizer: Tokenizer): AssignmentBlock = {
    // get the id of the variable


    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "int"
    tokenizer.nextToken // skip "="
    val value: String = tokenizer.nextToken.token
    new AssignmentBlock(superBlock,true,name, value)
  }

}
