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

package compiler.structure.blocks.variable

import compiler.structure.blocks.Block
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils

class VariableBlock(superBlockInit: Block, name: String) extends Block(superBlockInit, false, true) {

  println(name)
  val varType = SymbolTable.getInstance.getValue(Utils.getMethod(this).get, name).getType

  override def init() {}

  override def getName: String = name

  override def getValue: String = ""

  override def getType(): String = varType

  override def getOpeningCode: String = {
    if (Utils.getMethod(this) != null) {


      // if integer
      varType match {
        case "int" => asm.visitVarInsn("ILOAD", id) + expressions(0).getOpeningCode + asm.visitVarInsn("ISTORE", id)
        case "double" => asm.visitVarInsn("DLOAD", id) + expressions(0).getOpeningCode + asm.visitVarInsn("DSTORE", id)
        case "float" => asm.visitVarInsn("FLOAD", id) + expressions(0).getOpeningCode + asm.visitVarInsn("FSTORE", id)
        case "short" => asm.visitVarInsn("SALOAD", id) + expressions(0).getOpeningCode + asm.visitVarInsn("SASTORE", id)
        case "boolean" => asm.visitVarInsn("BALOAD", id) + expressions(0).getOpeningCode + asm.visitVarInsn("BASTORE", id)
        case default => ""
      }


    } else {
      ""
    }
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "variable: " + name

}