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

package compiler.structure.parsers.structures

import compiler.structure.blocks.Block
import compiler.structure.blocks.structures.MethodCallBlock
import compiler.structure.parsers.Parser
import compiler.tokenizer.Tokenizer

/**
  * Calls a method inside a class
  */
class MethodCallParser extends Parser[MethodCallBlock] {

  def shouldParse(line: String): Boolean =  line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*\\(\\)[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): MethodCallBlock = {
    val name: String = tokenizer.nextToken.token // Get the string value of the next token.
    return new MethodCallBlock(superBlock, name, "method_call", null)
  }
}
