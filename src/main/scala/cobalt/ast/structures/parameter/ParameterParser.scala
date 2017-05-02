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

package cobalt.ast.structures.parameter

import cobalt.ast.{Block, Parser}
import cobalt.tokenizer.Tokenizer

class ParameterParser extends Parser[ParameterBlock] {

  /**
    * A list of all regular stack
    *
    * @return
    */
  override val regex: String = "(public[ ]+|protected[ ]+|internal[ ]+)?(mutable[ ]+)?[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*[a-zA-Z][a-zA-Z0-9]*[ ]*"


  def parse(superBlock: Block, tokenizer: Tokenizer): ParameterBlock = {

    val nextToken: String = tokenizer.nextToken.token

    val mutable: Boolean = nextToken == "mut"

    // "val" or "var"
    val name: String = if (mutable) tokenizer.nextToken.token else nextToken
    tokenizer.nextToken

    val varType = tokenizer.nextToken.token

    new ParameterBlock(superBlock, mutable, name, varType)
  }
}
