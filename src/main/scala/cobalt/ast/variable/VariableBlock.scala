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

package cobalt.ast.variable

import cobalt.ast.Block
import cobalt.symbol_table.{Row, SymbolTable}
import cobalt.utilities.Utils

/**
  * Represents a variable reference
  *
  * @param superBlockInit
  * @param name
  */
class VariableBlock(superBlockInit: Block, name: String) extends Block(superBlockInit, false, true) {

  val row: Row = SymbolTable.getInstance.getValue(Utils.getMethod(this).get, name)

  override val getName: String = name

  override val getValue: String = ""

  override val getType: String = row.getType

  override def getOpeningCode(): String = {
    if (Utils.getMethod(this) != null) {

      val unwrap = if(superBlockInit.superBlock.isInstanceOf[DefineVariableBlock]) Utils.unwrapCode(this) else ""
      row.getType match {

        // Primitives
        case "char" => "mv.visitVarInsn(ILOAD" + "," + row.getId + ");\n"
        case "byte" => "mv.visitVarInsn(ILOAD" + "," + row.getId + ");\n"
        case "short" => "mv.visitVarInsn(ILOAD" + "," + row.getId + ");\n"
        case "int" => "mv.visitVarInsn(ILOAD" + "," + row.getId + ");\n"
        case "long" => "mv.visitVarInsn(LLOAD" + "," + row.getId + ");\n"
        case "float" => "mv.visitVarInsn(FLOAD" + "," + row.getId + ");\n"
        case "double" => "mv.visitVarInsn(DLOAD" + "," + row.getId + ");\n"

        // Objects
        case "Char" => "mv.visitVarInsn(ALOAD" + "," + row.getId + ");\n" + unwrap
        case "Byte" => "mv.visitVarInsn(ALOAD" + "," + row.getId + ");\n" + unwrap
        case "Short" => "mv.visitVarInsn(ALOAD" + "," + row.getId + ");\n" + unwrap
        case "Int" => "mv.visitVarInsn(ALOAD" + "," + row.getId + ");\n" + unwrap
        case "Long" => "mv.visitVarInsn(ALOAD" + "," + row.getId + ");\n" + unwrap
        case "Float" => "mv.visitVarInsn(ALOAD" + "," + row.getId + ");\n" + unwrap
        case "Double" => "mv.visitVarInsn(ALOAD" + "," + row.getId + ");\n" + unwrap
        case "String" => "mv.visitVarInsn(ALOAD" + "," + row.getId + ");\n" + unwrap
        case _ => "mv.visitVarInsn(ALOAD" + "," + row.getId + ");\n" + stack.map(_.getOpeningCode).mkString("\n")
      }
    }
    else {
      ""
    }
  }


  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "variable: " + name + " " + stack

}