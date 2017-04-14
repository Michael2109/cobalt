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
        case "Double" => "mv.visitTypeInsn(NEW, \"java/lang/Double\");\n" + "mv.visitInsn(DUP);\n" + ReversePolish.infixToRPN(stack.drop(1).toList).map(b => b.getOpeningCode).mkString("\n")
        case "Short" => "mv.visitTypeInsn(NEW, \"java/lang/Short\");\n" + "mv.visitInsn(DUP);\n" + ReversePolish.infixToRPN(stack.drop(1).toList).map(b => b.getOpeningCode).mkString("\n")
        case "Long" => "mv.visitTypeInsn(NEW, \"java/lang/Long\");\n" + "mv.visitInsn(DUP);\n" + ReversePolish.infixToRPN(stack.drop(1).toList).map(b => b.getOpeningCode).mkString("\n")
        case _ => "mv.visitTypeInsn(NEW, \"" + Utils.getDirectory(this, varType) + "/" + varType + "\");\n" + "mv.visitInsn(DUP);\n"

      }

    } else {
      ""
    }
  }


  override def getClosingCode: String = {


    varType match {
      case "Int" => "mv.visitMethodInsn(INVOKESPECIAL, \"java/lang/Integer\", \"<init>\", \"(I)V\", false);\n" + asm.visitVarInsn("ASTORE", id)
      case "Float" => "mv.visitMethodInsn(INVOKESPECIAL, \"java/lang/Float\", \"<init>\", \"(F)V\", false);\n" + asm.visitVarInsn("ASTORE", id)
      case "Double" => "mv.visitMethodInsn(INVOKESPECIAL, \"java/lang/Double\", \"<init>\", \"(D)V\", false);\n" + asm.visitVarInsn("ASTORE", id)
      case "Short" => "mv.visitMethodInsn(INVOKESPECIAL, \"java/lang/Short\", \"<init>\", \"(S)V\", false);\n" + asm.visitVarInsn("ASTORE", id)
      case "Long" => "mv.visitMethodInsn(INVOKESPECIAL, \"java/lang/Long\", \"<init>\", \"(L)V\", false);\n" + asm.visitVarInsn("ASTORE", id)
      case "String" => asm.visitVarInsn("ASTORE", id)
      case _ => "mv.visitMethodInsn(INVOKESPECIAL, \"" + Utils.getDirectory(this, varType) + "/" + varType + "\", \"<init>\", \"()V\", false);\n" + asm.visitVarInsn("ASTORE", id)

    }

  }

  override def toString: String = "def variable: " + name + stack

}