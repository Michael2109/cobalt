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

package compiler.structure.blocks.loops

import compiler.structure.blocks.Block
import compiler.structure.generators.loops.WhileGen
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils

class WhileBlock(var superBlockInit: Block, var name: String) extends Block(superBlockInit, true, false) {

  val split: Array[String] = name.split(" ")
  private var pointer: String = null
  private var operator: String = null
  private var value: String = null
  private var byteCodeOp: String = ""

  def getName: String = name

  def getValue: String = null

  def getType: String = "while"

  def init(): Unit = {

    //  x == 10
    if (split.length > 1) {
      pointer = split(0)

      pointer = "" + SymbolTable.getInstance.getValue(Utils.getMethod(this), split(0)).getId
      operator = split(1)
      value = split(2)

      if (operator == "==") {
        byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
      }
      else if (operator == "<") {
        byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
      }
      else if (operator == ">") {
        byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
      }
      else if (operator == "<=") {
        byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
      }
      else if (operator == ">=") {
        byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
      }
      else {
        System.out.println("Error: Disallowed Operator" + this.getClass)
      }
    }
    else {
      //boolean value
      value = name
    }
  }

  def getOpeningCode: String = {
     WhileGen.getOpeningCode(id, pointer, value, byteCodeOp)
  }

  def getClosingCode: String = {
    WhileGen.getClosingCode(id)
  }

  override def toString: String = "while: " + name

}