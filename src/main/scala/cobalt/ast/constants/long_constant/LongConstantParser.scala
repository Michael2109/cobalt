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

package cobalt.ast.constants.long_constant

import cobalt.ast.Block
import cobalt.ast.constants.ConstantParser
import cobalt.tokenizer.Tokenizer

class LongConstantParser extends ConstantParser[LongConstantBlock] {


  /**
    * A list of all regular stack
    *
    * @return
    */
  override val regex: String = "^((-)?[0-9]+(l|L))"


  override def parse(superBlock: Block, tokenizer: Tokenizer): LongConstantBlock = {
    val value: String = tokenizer.nextToken.token
    new LongConstantBlock(superBlock, value)
  }

}
