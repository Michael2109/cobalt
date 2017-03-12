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

package compiler.structure.blocks.structures

import compiler.data.parameters.Parameter
import compiler.structure.blocks.Block
import compiler.structure.blocks.imports.ImportBlock
import compiler.structure.blocks.packages.PackageBlock
import compiler.structure.blocks.structures.kinds.{ClassBlock, ObjectBlock}
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils

import scala.collection.mutable.ListBuffer

/**
  * Calling a method of an object
  */
class ObjectMethodCallBlock(var superBlockInit: Block, var variableName: String, var methodName: String, var params: ListBuffer[Parameter]) extends Block(superBlockInit, false, false, false) {

  id_=(SymbolTable.getInstance.getValue(Utils.getMethod(this).get, variableName).getId)
  private val `type`: String = null
  private val className: String = SymbolTable.getInstance.getValue(Utils.getMethod(this).get, variableName).getType
  private var parameterString: String = ""
  private var argumentString: String = ""
  private var directory: String = ""

  def getParameters: ListBuffer[Parameter] = {
    params
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
      println(Utils.getMethod(this) + " " + param.getName)
      param.setType(SymbolTable.getInstance.getValue(Utils.getMethod(this).get, param.getName).getType)
      parameterString += param.getAsmType
      argumentString += "mv.visitIntInsn(ALOAD, " + SymbolTable.getInstance.getValue(Utils.getMethod(this).get, param.getName).getId + ");"
    }
  }

  // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
  def getDirectory: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!block.isInstanceOf[FileBlock]) {
      {
        block = block.superBlock
      }
    }
    // Get the directory of the Object
    for (sub <- block.subBlocks) {
      sub match {
        case block1: ImportBlock if block1.fileName == className =>
          return block1.directory
        case _ =>
      }
    }
    ""
  }

  // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
  def getPackage: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!block.isInstanceOf[FileBlock]) {
      {
        block = block.superBlock
      }
    }
    // Get the directory of the Object
    for (sub <- block.subBlocks) {
      sub match {
        case block1: PackageBlock =>
          return block1.directory
        case _ =>
      }
    }
    ""
  }

  // Returns the main class name for the file
  def getClassName: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!block.isInstanceOf[ClassBlock] && !block.isInstanceOf[ObjectBlock]) {
      {
        block = block.superBlock
      }
    }

    // Get the directory of the Object
    block.getName
  }


  def getOpeningCode: String = {
    "mv.visitVarInsn(ALOAD, " + id + ");\n" + argumentString + "mv.visitMethodInsn(INVOKEVIRTUAL, \"" + directory + "/" + className + "\", \"" + methodName + "\", \"(" + parameterString + ")V\", false);\n"
  }

  def getClosingCode: String = {
    ""
  }

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    "object method call: " + variableName + " ( " + paramString + ")"
  }
}