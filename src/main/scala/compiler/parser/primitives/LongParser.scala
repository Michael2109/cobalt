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

package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.LongBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class LongParser extends Parser[LongBlock]{

  /**
    * Takes a line and checks to see ifs it is for this parser by using regex.
    */

  //var longTest:long = 10
  override def shouldParse(line: String): Boolean = line.matches("(val|var)[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*long[ ]*([=][ ]*[0-9]+[ ]*)?")

  /**
    * Take the superBlock and the tokenizer for the line and return a block of this parser's type.
    */
  override def parse(superBlock: Block, tokenizer: Tokenizer): LongBlock = {
    val declaration: Boolean = tokenizer.nextToken.token == "val" // "val" or "var"
    val name = tokenizer.nextToken.token // longTest
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "long"
    tokenizer.nextToken // skip "="
    var value = tokenizer.nextToken.token // 10

    if(value.equals(""))
      value = "0"

    new LongBlock(superBlock, declaration,name, value)
  }
}