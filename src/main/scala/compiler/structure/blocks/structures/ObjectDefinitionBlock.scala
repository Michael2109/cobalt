/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Michael Haywood
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

package compiler.structure.blocks.structures

import compiler.data.parameters.Parameter
import compiler.structure.blocks.Block
import compiler.structure.blocks.structures.kinds.{ClassBlock, ObjectBlock}
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils


// Creation of a new object and storing to a variable
class ObjectDefinitionBlock(superBlockInit: Block, declaration : Boolean, className: String, variableName: String, operator: String, newKeyword: String, initClassName: String, params: Array[Parameter]) extends Block(superBlockInit, false, true ,false) {

  private var parameterString: String = ""
  private var argumentString: String = ""
  private var directory: String = ""

  def init() {
    if (className == getObjectName) {
      directory = Utils.getPackage(this)
    }
    else {
      directory = Utils.getDirectory(this, className)
    }

    // Get the type of the parameters
    for (param <- params) {
      param.setType(SymbolTable.getInstance.getValue(Utils.getMethod(this), param.getName).getType)
      parameterString += param.getAsmType
      argumentString += "mv.visitIntInsn(ILOAD, " + SymbolTable.getInstance.getValue(Utils.getMethod(this), param.getName).getId + ");"
    }
  }





  // Returns the main class name for the file
  def getObjectName: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!(block.isInstanceOf[ClassBlock]) && !(block.isInstanceOf[ObjectBlock])) {
        block = block.superBlock
    }
    // Get the directory of the Object
    return block.getName
  }

  def getName: String = variableName

  def getValue: String = ""

  def getType: String = className

  def getOpeningCode: String = {
    return "mv.visitTypeInsn(NEW, \"" + directory + (if (directory == "") ""
    else "/") + className + "\");\n" + "mv.visitInsn(DUP);\n" + argumentString + "mv.visitMethodInsn(INVOKESPECIAL, \"" + directory + (if (directory == "") ""
    else "/") + className + "\", \"<init>\", \"(" + parameterString + ")V\", false);\n" + "mv.visitVarInsn(ASTORE," + id + ");\n"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "object: " + className + " " + variableName + " = new " + initClassName + "()"

}