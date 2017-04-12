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

/**
  * Represents a variable definition
  *
  * @param superBlockInit
  * @param declaration
  * @param name
  * @param varType
  */
class DefineVariableBlock(superBlockInit: Block, declaration: Boolean, name: String, varType: String) extends Block(superBlockInit, false, true) {

  SymbolTable.getInstance.addRow(new Row().setId(id).setName(getName).setType(getType).setValue(getValue).setMethodName(Utils.getMethod(this).get.getName).setClassName(Utils.getClass(this).get.getName))

  override def getName: String = name

  override def getValue: String = ""

  // Map the defined var type to the ASM type
  override def getType: String = varType

  override def getOpeningCode: String = {
    if (Utils.getMethod(this) != null) {

      varType match {
        case "Int" => "mv.visitTypeInsn(NEW, \"java/lang/Integer\");\n" + "mv.visitInsn(DUP);\n" + ReversePolish.infixToRPN(stack.drop(1).toList).map(b => b.getOpeningCode).mkString("\n")
        case "Float" => "mv.visitTypeInsn(NEW, \"java/lang/Float\");\n" + "mv.visitInsn(DUP);\n" + ReversePolish.infixToRPN(stack.drop(1).toList).map(b => b.getOpeningCode).mkString("\n")

        case _ => if (stack.nonEmpty && stack.head.isInstanceOf[AssignmentOpBlock]) ReversePolish.infixToRPN(stack.drop(1).toList).map(b => b.getOpeningCode).mkString("\n") else ""
      }

    } else {
      ""
    }
  }


  override def getClosingCode: String = {


    varType match {
      case "Int" => "mv.visitMethodInsn(INVOKESPECIAL, \"java/lang/Integer\", \"<init>\", \"(I)V\", false);\n" + asm.visitVarInsn("ASTORE", id)
      case "Float" => "mv.visitMethodInsn(INVOKESPECIAL, \"java/lang/Float\", \"<init>\", \"(F)V\", false);\n" + asm.visitVarInsn("ASTORE", id)

    }

  }

  override def toString: String = "def variable: " + name + stack

}