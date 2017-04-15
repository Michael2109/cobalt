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
  * Represents a variable reference
  *
  * @param superBlockInit
  * @param name
  */
class VariableBlock(superBlockInit: Block, name: String) extends Block(superBlockInit, false, true) {

  val row: Row = SymbolTable.getInstance.getValue(Utils.getMethod(this).get, name)

  override def getName: String = name

  override def getValue: String = ""

  override def getType: String = row.getType

  override def getOpeningCode: String = {
    if (Utils.getMethod(this) != null) {

      // Get assigned blocks in reverse polish notation
      val rpnString: String = if (stack.nonEmpty && stack.head.isInstanceOf[AssignmentOpBlock]) ReversePolish.infixToRPN(stack.drop(1).toList).map(b => b.getOpeningCode).mkString("\n") else ""

        row.getType match {

          // Primitives
          case "char" => asm.visitVarInsn("ILOAD", "" + row.getId)
          case "byte" => asm.visitVarInsn("ILOAD", "" + row.getId)
          case "short" => asm.visitVarInsn("ILOAD", "" + row.getId)
          case "int" => asm.visitVarInsn("ILOAD", "" + row.getId)
          case "long" => asm.visitVarInsn("LLOAD", "" + row.getId)
          case "float" => asm.visitVarInsn("FLOAD", "" + row.getId)
          case "double" => asm.visitVarInsn("DLOAD", "" + row.getId)

          // Objects
          case "Char" => asm.visitVarInsn("ALOAD", "" + row.getId) + "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/lang/Character\", \"charValue\", \"()C\", false);\n"
          case "Byte" => asm.visitVarInsn("ALOAD", "" + row.getId) + "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/lang/Byte\", \"byteValue\", \"()B\", false);\n"
          case "Short" => asm.visitVarInsn("ALOAD", "" + row.getId) + "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/lang/Short\", \"shortValue\", \"()S\", false);\n"
          case "Int" => asm.visitVarInsn("ALOAD", "" + row.getId) + "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/lang/Integer\", \"intValue\", \"()I\", false);\n"
          case "Long" => asm.visitVarInsn("ALOAD", "" + row.getId) + "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/lang/Long\", \"longValue\", \"()J\", false);\n"
          case "Float" => asm.visitVarInsn("ALOAD", "" + row.getId) + "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/lang/Float\", \"floatValue\", \"()F\", false);\n"
          case "Double" => asm.visitVarInsn("ALOAD", "" + row.getId) + "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/lang/Double\", \"doubleValue\", \"()D\", false);\n"
          case "String" => asm.visitVarInsn("ALOAD", "" + row.getId)
          case _ => asm.visitVarInsn("ALOAD", "" + row.getId) + stack.map(_.getOpeningCode).mkString("\n")

        }


    } else {
      ""
    }
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "variable: " + name + " " + stack

}