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

package cobalt.ast.constants.boolean_constant

import cobalt.ast.Block
import cobalt.ast.constants.ConstantBlock
import cobalt.utilities.Utils

/**
  * Represents a "boolean" constant
  *
  * @param superBlockInit
  * @param value
  */
class BooleanConstantBlock(var superBlockInit: Block, value: String) extends ConstantBlock(superBlockInit, false, true) {

  override val getName: String = ""

  override val getValue: String = value

  override val getType: String = "boolean"

  override def getOpeningCode: String = {
    if (Utils.getMethod(this) != null) {
      "mv.visitInsn(" + "ICONST_" + (if (value == "true") "1" else "0") + ");"
    } else {
      ""
    }
  }

  override def getClosingCode: String = ""

  override def toString: String = getType + ": " + value

}