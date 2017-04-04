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

package compiler.ast.parsers.push

import compiler.ast.blocks.Block
import compiler.ast.blocks.push.PushBlock
import compiler.ast.parsers.Parser
import compiler.tokenizer.Tokenizer

class PushParser extends Parser[PushBlock] {
  val printVariable: Boolean = false

  /**
    * A list of all regular stack
    *
    * @return
    */
  override def getRegexs: List[String] = List(
    // "([a-z][a-zA-Z0-9]*)|([0-9]+)"
  )

  def parse(superBlock: Block, tokenizer: Tokenizer): PushBlock = {
    val value = tokenizer.nextToken.token // skip print

    new PushBlock(superBlock, value, printVariable)
  }
}
