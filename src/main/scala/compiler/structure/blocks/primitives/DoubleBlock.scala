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

package compiler.structure.blocks.primitives

import compiler.structure.blocks.Block
import compiler.symbol_table.{Row, SymbolTable}
import compiler.utilities.Utils

class DoubleBlock(superBlockInit: Block, declaration : Boolean, name: String,  value: String) extends Block(superBlockInit, false, true) {

  println("Gettomng ,theopd")
  println(Utils.getMethod(this))
  SymbolTable.getInstance.addRow(new Row().setId(id).setName(getName).setType(getType).setValue(getValue).setMethodName(Utils.getMethod(this).get.getName).setClassName(Utils.getClass(this).getName))

  override def init() {}

  override def getName: String = name

  override  def getValue: String = value

  override def getType(): String = "double"

  override def getOpeningCode: String = {
    if (Utils.getMethod(this) != null) {
      "mv.visitLdcInsn(new Double(" + value + "));\n" +
        "mv.visitVarInsn(DSTORE, " + id + ");"
    } else {
      ""
    }
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "double: " + name + " = " + value

}