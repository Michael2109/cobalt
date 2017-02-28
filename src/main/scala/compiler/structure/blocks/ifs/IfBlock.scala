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

package compiler.structure.blocks.ifs

import compiler.data.parameters.Parameter
import compiler.structure.blocks.Block
import compiler.structure.generators.ifs.IfGen
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils

/**
  * Represents an if statement
  *
  * @param superBlockInit
  * @param nameInit
  */
class IfBlock(var superBlockInit: Block, var nameInit: String) extends Block(superBlockInit, true, false) {

  private val params: Array[Parameter] = null
  private val _name = nameInit
  private val split: Array[String] = _name.split(" ")
  private val pointer: String = "" + SymbolTable.getInstance.getValue(Utils.getMethod(this), split(0)).getId
  private val operator: String = if(split.length > 1) split(1) else ""
  private val value: String = if(split.length > 1) split(2) else _name

  private val byteCodeOp: String = {
    if (operator == "==") {
      "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == "<") {
      "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == ">") {
      "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == "<=") {
      "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == ">=") {
      "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else {
      throw new RuntimeException("Error: Disallowed Operator")
    }
  }

  def getParameters: Array[Parameter] = params

  def getName: String = _name

  def getType: String = "if"

  def getValue: String = null

  def init() {
  }

  def getOpeningCode: String = {
    return IfGen.getOpeningCode(pointer, value, id, byteCodeOp)
  }

  def getClosingCode: String = {
    return asm.visitLabel("l" + id)
  }

  override def toString: String = "if " + _name

}