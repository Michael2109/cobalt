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

package compiler.ast.generators.ifs

import compiler.ast.blocks.conditionals.AbstractConditionalBlock
import compiler.ast.blocks.ifs.IfBlock
import compiler.ast.generators.Generator

object IfGen extends Generator{

  def getOpeningCode(ifBlock: IfBlock): String = {

    val values = ifBlock.orderedStatementBlocks.filter(!_.isInstanceOf[AbstractConditionalBlock]).map(_.getOpeningCode).mkString("")

    values +
      asm.newLabel("l" + ifBlock.id) +
      asm.visitJumpInsn(ifBlock.orderedStatementBlocks.filter(_.isInstanceOf[AbstractConditionalBlock]).head.getOpeningCode, "l" + ifBlock.id)

  }

  def getClosingCode(ifBlock: IfBlock): String = {
    asm.visitLabel("l" + ifBlock.id)
  }

}