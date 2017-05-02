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

package cobalt.ast.constants.byte_constant

import cobalt.ast.Block
import cobalt.ast.constants.ConstantBlock
import cobalt.utilities.Utils

/**
  * Represents a "byte" constant
  *
  * @param superBlockInit
  * @param value
  */
class ByteConstantBlock(var superBlockInit: Block, value: String) extends ConstantBlock(superBlockInit, false, true) {

  override def getName: String = ""

  override def getValue: String = value

  override def getType: String = "byte"

  override def getOpeningCode: String = {
    if (Utils.getMethod(this) != null) {
      "mv.visitLdcInsn(" + "new Byte(\"" + value.replaceAll("(b|B)", "") + "\")" + ");\n"
    } else {
      ""
    }
  }

  override def getClosingCode: String = ""

  override def toString: String = getType + ": " + value


}