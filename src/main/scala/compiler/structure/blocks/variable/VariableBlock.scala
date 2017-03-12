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
import compiler.structure.blocks.operators.AssignmentBlock
import compiler.symbol_table.{Row, SymbolTable}
import compiler.utilities.{ReversePolish, Utils}


class VariableBlock(superBlockInit: Block, name: String) extends Block(superBlockInit, false, true) {

  val row: Row = SymbolTable.getInstance.getValue(Utils.getMethod(this).get, name)

  override def init() {}

  override def getName: String = name

  override def getValue: String = ""

  override def getType: String = row.getType

  override def getOpeningCode: String = {
    if (Utils.getMethod(this) != null) {

      // Get assigned blocks in reverse polish notation
      val rpnString: String = if (expressions.nonEmpty && expressions.head.isInstanceOf[AssignmentBlock]) ReversePolish.infixToRPN(expressions.drop(1).toList).map(b => b.getOpeningCode).mkString("\n") else ""

      if (expressions.isEmpty) {
        row.getType match {
          case "int" => asm.visitVarInsn("ILOAD", "" + row.getId)
          case "double" => asm.visitVarInsn("DLOAD", "" + row.getId)
          case "float" => asm.visitVarInsn("FLOAD", "" + row.getId)
          case "short" => asm.visitVarInsn("SALOAD", "" + row.getId)
          case "boolean" => asm.visitVarInsn("BALOAD", "" + row.getId)
          case _ => ""
        }
      }
      else {
        row.getType match {
          case "int" => rpnString + asm.visitVarInsn("ISTORE", "" + row.getId)
          case "double" => rpnString + asm.visitVarInsn("DSTORE", "" + row.getId)
          case "float" => rpnString + asm.visitVarInsn("FSTORE", "" + row.getId)
          case "short" => rpnString + asm.visitVarInsn("SASTORE", "" + row.getId)
          case "boolean" => rpnString + asm.visitVarInsn("BASTORE", "" + row.getId)
          case "String" => rpnString + asm.visitVarInsn("ASTORE", "" + row.getId)

          case _ => ""
        }
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