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

package compiler.ast.blocks.variable

import compiler.ast.blocks.Block
import compiler.ast.blocks.operators.assignment.AssignmentOpBlock
import compiler.symbol_table.{Row, SymbolTable}
import compiler.utilities.{ReversePolish, Utils}


class VariableBlock(superBlockInit: Block, name: String) extends Block(superBlockInit, false, true) {

  val row: Row = SymbolTable.getInstance.getValue(Utils.getMethod(this).get, name)

  override def getName: String = name

  override def getValue: String = ""

  override def getType: String = row.getType

  override def getOpeningCode: String = {
    if (Utils.getMethod(this) != null) {

      // Get assigned blocks in reverse polish notation
      val rpnString: String = if (stack.nonEmpty && stack.head.isInstanceOf[AssignmentOpBlock]) ReversePolish.infixToRPN(stack.drop(1).toList).map(b => b.getOpeningCode).mkString("\n") else ""

      if (stack.isEmpty) {
        row.getType match {
          case "C" => asm.visitVarInsn("ILOAD", "" + row.getId)
          case "B" => asm.visitVarInsn("ILOAD", "" + row.getId)
          case "I" => asm.visitVarInsn("ILOAD", "" + row.getId)
          case "D" => asm.visitVarInsn("DLOAD", "" + row.getId)
          case "F" => asm.visitVarInsn("FLOAD", "" + row.getId)
          case "S" => asm.visitVarInsn("ILOAD", "" + row.getId)
          case "J" => asm.visitVarInsn("LLOAD", "" + row.getId)
          case "Z" => asm.visitVarInsn("ILOAD", "" + row.getId)
          case "Ljava/lang/String;" => asm.visitVarInsn("ALOAD", "" + row.getId)
          case _ => ""
        }
      }
      else {
        row.getType match {
          case "I" => rpnString + asm.visitVarInsn("ISTORE", "" + row.getId)
          case "D" => rpnString + asm.visitVarInsn("DSTORE", "" + row.getId)
          case "F" => rpnString + asm.visitVarInsn("FSTORE", "" + row.getId)
          case "S" => rpnString + asm.visitVarInsn("SASTORE", "" + row.getId)
          case "Z" => rpnString + asm.visitVarInsn("BASTORE", "" + row.getId)
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