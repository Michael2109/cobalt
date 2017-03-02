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

package compiler.structure.blocks.push

import compiler.structure.blocks.Block
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils

class PushBlock(var superBlockInit: Block, var value: String, val isVariableInit: Boolean) extends Block(superBlockInit, false, false) {

  val pushId = SymbolTable.getInstance.getValue(Utils.getMethod(this).get, value).getId
  // Called after file is parsed
  override def init(): Unit = {}

  /* Symbol table information */
  override def getName: String = ""

  override def getValue: String = value

  override def getType: String = "PUSH"

  /* Bytecode for the opening and closing of the blocks */
  override def getOpeningCode: String = {
    ""
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "push " + value + " " + pushId
}
