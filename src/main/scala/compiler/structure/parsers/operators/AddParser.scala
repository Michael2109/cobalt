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

package compiler.structure.parsers.operators

import compiler.structure.blocks.Block
import compiler.structure.blocks.operators.AddBlock
import compiler.structure.parsers.Parser
import compiler.tokenizer.Tokenizer

class AddParser extends Parser[AddBlock] {
  def shouldParse(line: String): Boolean = line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*[+][=][ ]*[0-9]+")

  def parse(superBlock: Block, tokenizer: Tokenizer): AddBlock = {
    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken
    val value: String = tokenizer.nextToken.token
    new AddBlock(superBlock, name, value)
  }
}
