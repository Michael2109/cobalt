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

package compiler.structure.blocks.operators

import compiler.structure.blocks.Block
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils

/**
  * Represents adding a value to a primitive
  *
  * @param superBlockInit
  * @param name
  * @param valueInit
  */
class AddBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, false) {

  def init() {
    id = (new Integer(SymbolTable.getInstance.getValue(Utils.getMethod(this), name).getId))
  }

  def getName: String = name

  def getType: String = "add"

  def getOpeningCode: String = {
  //  asm.visitLdcInsn("new Integer("+this.getValue+");")

     "mv.visitIincInsn(" + id + ", " + this.getValue + ");"
  }

  def getValue: String = value

  def getClosingCode: String = {
     ""
  }

  override def toString: String = "add: " + name

}