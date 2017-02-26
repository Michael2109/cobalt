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
import compiler.structure.blocks.imports.ImportBlock
import compiler.structure.blocks.packages.PackageBlock
import compiler.structure.blocks.structures.kinds.{ClassBlock, ObjectBlock}
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils

/**
  * Calling a method of an object
  */
class ObjectMethodCallBlock(var superBlockInit: Block, var variableName: String, var methodName: String, var params: Array[Parameter]) extends Block(superBlockInit, false, false, false) {

  id_=(SymbolTable.getInstance.getValue(Utils.getMethod(this), variableName).getId)
  private val `type`: String = null
  private val className: String = SymbolTable.getInstance.getValue(Utils.getMethod(this), variableName).getType
  private var parameterString: String = ""
  private var argumentString: String = ""
  private var directory: String = ""

  def getParameters: Array[Parameter] = {
    return params
  }

  def getName: String = variableName

  def getValue: String = ""

  def getType: String = `type`

  def init() {
    if (className == getClassName)
      directory = getPackage
    else
      directory = getDirectory

    // Get the type of the parameters
    for (param <- params) {
      param.setType(SymbolTable.getInstance.getValue(Utils.getMethod(this), param.getName).getType)
      parameterString += param.getAsmType
      argumentString += "mv.visitIntInsn(ALOAD, " + SymbolTable.getInstance.getValue(Utils.getMethod(this), param.getName).getId + ");"
    }
  }

  // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
  def getDirectory: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!(block.isInstanceOf[FileBlock])) {
      {
        block = block.superBlock
      }
    }
    // Get the directory of the Object
    for (sub <- block.subBlocks) {
      if (sub.isInstanceOf[ImportBlock] && (sub.asInstanceOf[ImportBlock]).fileName == className) {
        return (sub.asInstanceOf[ImportBlock]).directory
      }
    }
    return ""
  }

  // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
  def getPackage: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!(block.isInstanceOf[FileBlock])) {
      {
        block = block.superBlock
      }
    }
    // Get the directory of the Object
    for (sub <- block.subBlocks) {
      if (sub.isInstanceOf[PackageBlock]) {
        return (sub.asInstanceOf[PackageBlock]).directory
      }
    }
    return ""
  }

  // Returns the main class name for the file
  def getClassName: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!(block.isInstanceOf[ClassBlock]) && !(block.isInstanceOf[ObjectBlock])) {
      {
        block = block.superBlock
      }
    }

    // Get the directory of the Object
    return block.getName
  }


  def getOpeningCode: String = {
    return "mv.visitVarInsn(ALOAD, " + id + ");\n" + argumentString + "mv.visitMethodInsn(INVOKEVIRTUAL, \"" + directory + "/" + className + "\", \"" + methodName + "\", \"(" + parameterString + ")V\", false);\n"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return "object method call: " + variableName + " ( " + paramString + ")"
  }
}