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

package compiler.ast.blocks.structures

import compiler.ast.blocks.Block
import compiler.ast.blocks.packages.PackageBlock
import compiler.ast.blocks.structures.kinds.{ClassBlock, ObjectBlock}
import compiler.data.parameters.Parameter
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils

import scala.collection.mutable.ListBuffer

/**
  * Represents calling a method in an object
  * E.g. obj.methodCall(10,20)
  */
class ObjectMethodCallBlock(var superBlockInit: Block, var methodName: String, var params: ListBuffer[Parameter]) extends Block(superBlockInit, false, false, false) {

  private val `type`: String = null
  // private val className: String = SymbolTable.getInstance.getValue(Utils.getMethod(this).get, variableName).getType
  private var parameterString: String = params.map(p => p.getType).mkString(";")
  private var argumentString: String = ""
  private var directory: String = ""

  params.foreach(p => p.setType(SymbolTable.getInstance.getValue(Utils.getMethod(this).get, p.getName).getType))

  def getParameters: ListBuffer[Parameter] = {
    params
  }


  /*
        // Get the type of the parameters
        for (param <- params) {
          println(Utils.getMethod(this) + " " + param.getName)
          param.setType(SymbolTable.getInstance.getValue(Utils.getMethod(this).get, param.getName).getType)
          parameterString += param.getType
          argumentString += "mv.visitIntInsn(ALOAD, " + SymbolTable.getInstance.getValue(Utils.getMethod(this).get, param.getName).getId + ");"
        }
  */



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

  override def getName: String = ""

  override def getValue: String = ""

  override def getType: String = `type`

  override def getOpeningCode: String = {
    // "mv.visitVarInsn(ALOAD, " + id + ");\n" + argumentString + "mv.visitMethodInsn(INVOKEVIRTUAL, \"" + directory + "/" + className + "\", \"" + methodName + "\", \"(" + parameterString + ")V\", false);\n"
    ""
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    "object method call: " + methodName + " ( " + paramString + ")"
  }
}