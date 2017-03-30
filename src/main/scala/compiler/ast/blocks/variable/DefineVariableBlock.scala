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

class DefineVariableBlock(superBlockInit: Block, declaration: Boolean, name: String, varType: String) extends Block(superBlockInit, false, true) {

  SymbolTable.getInstance.addRow(new Row().setId(id).setName(getName).setType(getType).setValue(getValue).setMethodName(Utils.getMethod(this).get.getName).setClassName(Utils.getClass(this).get.getName))

  override def getName: String = name

  override def getValue: String = ""

  override def getOpeningCode: String = {
    if (Utils.getMethod(this) != null) {

      // Get assigned blocks in reverse polish notation
      val rpnString: String = if (expressions.nonEmpty && expressions.head.isInstanceOf[AssignmentOpBlock]) ReversePolish.infixToRPN(expressions.drop(1).toList).map(b => b.getOpeningCode).mkString("\n") else ""

      // if integer
      getType match {
        case "C" => rpnString + asm.visitVarInsn("ISTORE", id)
        case "B" => rpnString + asm.visitVarInsn("ISTORE", id)
        case "I" => rpnString + asm.visitVarInsn("ISTORE", id)
        case "D" => rpnString + asm.visitVarInsn("DSTORE", id)
        case "F" => rpnString + asm.visitVarInsn("FSTORE", id)
        case "S" => rpnString + asm.visitVarInsn("ISTORE", id)
        case "Z" => rpnString + asm.visitVarInsn("ISTORE", id)
        case "J" => rpnString + asm.visitVarInsn("LSTORE", id)
        case "Ljava/lang/String;" => rpnString + asm.visitVarInsn("ASTORE", id)

        case _ => expressions.map(b => b.getOpeningCode).mkString("\n") + asm.visitVarInsn("ASTORE", id)
      }

    } else {
      ""
    }
  }

  override def getType: String = varType.trim match {

    case "char" => "C"
    case "byte" => "I"
    case "int" => "I"
    case "long" => "J"
    case "double" => "D"
    case "float" => "F"
    case "short" => "S"
    case "boolean" => "Z"
    case "String" => "Ljava/lang/String;"
    case _ => "void"
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "def variable: " + name + expressions

}