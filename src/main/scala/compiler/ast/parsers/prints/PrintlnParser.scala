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

package compiler.ast.parsers.prints

import compiler.ast.blocks.Block
import compiler.ast.blocks.prints.PrintlnBlock
import compiler.ast.parsers.Parser
import compiler.tokenizer.tokens.constants.StringLiteralToken
import compiler.tokenizer.{Token, Tokenizer}

// todo set up print parser
class PrintlnParser extends Parser[PrintlnBlock] {

  /**
    * A list of all regular stack
    *
    * @return
    */
  override def getRegexs: List[String] = List(
    "println[ ]*\\([\"].*[\"]\\)",
    "println[ ]*\\(.*\\)"
  )


  def parse(superBlock: Block, tokenizer: Tokenizer): PrintlnBlock = {
    tokenizer.nextToken // skip print
    tokenizer.nextToken
    // skip (
    val value: Token = tokenizer.nextToken
    val isVar: Boolean = !value.tokenType.isInstanceOf[StringLiteralToken]

    new PrintlnBlock(superBlock, value.token, isVar)
  }

}
