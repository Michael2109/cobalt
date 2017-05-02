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

package cobalt.ast.expression

import cobalt.ast.constants.boolean_constant.BooleanConstantParser
import cobalt.ast.constants.byte_constant.ByteConstantParser
import cobalt.ast.constants.char_constant.CharConstantParser
import cobalt.ast.constants.double_constant.DoubleConstantParser
import cobalt.ast.constants.float_constant.FloatConstantParser
import cobalt.ast.constants.int_constant.IntConstantParser
import cobalt.ast.constants.long_constant.LongConstantParser
import cobalt.ast.constants.short_constant.ShortConstantParser
import cobalt.ast.constants.string_constant.StringConstantParser
import cobalt.ast.operators._
import cobalt.ast.variable.VariableParser
import cobalt.ast.{Block, Parser}
import cobalt.tokenizer.Tokenizer
import cobalt.utilities.{RPN, Utils}

class ExpressionParser extends Parser[ExpressionBlock] {
  /**
    * A list of all regular stack
    *
    * @return
    */
  override val regex: String = "([\\(\\)]*[0-9]+([\\.][0-9]*)?([dDfFsSlLbB])?[ \\(\\)]*[\\+\\-\\*\\/][ \\(\\)]*)+[0-9]+([\\.][0-9]*)?([dDfFsSlLbB])?[ \\(\\)]*"

  def parse(superBlock: Block, tokenizer: Tokenizer): ExpressionBlock = {
    val parsers:List[Parser[_]] = List[Parser[_]](
      new AddOpParser,
      new DivideOpParser,
      new MultiplyOpParser,
      new SubtractOpParser,
      new ModulusOpParser,

      /* constants */
      new BooleanConstantParser,
      new CharConstantParser,
      new StringConstantParser,
      new FloatConstantParser,
      new DoubleConstantParser,
      new ShortConstantParser,
      new ByteConstantParser,
      new LongConstantParser,
      new IntConstantParser,

      new VariableParser,
      new OpeningBracketOpParser,
      new ClosingBracketOpParser
    )

    val expressionBlocks:List[Block] = RPN.infixToRPN(Utils.getAllBlocks(superBlock, tokenizer.line, 0, parsers))

    new ExpressionBlock(superBlock, expressionBlocks)
  }
}
