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
import compiler.utilities.Utils

/**
  * Represents calling a method in an object
  * E.g. obj.methodCall(10,20)
  */
class ObjectMethodCallBlock(var superBlockInit: Block, var methodName: String, var argumentBlocks: List[Block]) extends Block(superBlockInit, false, false, false) {

  private val argTypeString: String = argumentBlocks.map(b => Utils.getASMType(b.getType)).mkString("")
  private val argStackString: String = argumentBlocks.map(_.getOpeningCode).mkString("\n")


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
  val className: String = {
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

  override def getType: String = superBlockInit.getType

  override def getOpeningCode: String = {

    val directory = if (Utils.getDirectory(this, superBlockInit.getType) == "") Utils.getPackage(this).replace(".", "/") else Utils.getDirectory(this, superBlockInit.getType)

    argStackString +
      "mv.visitMethodInsn(INVOKEVIRTUAL, \"" + directory + "/" + superBlockInit.getType + "\", \"" + methodName + "\", \"(" + argTypeString + ")V\", false);\n"

  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = {
    "<OBJECT_METHOD_CALL> " + methodName + " ( " + argTypeString + ")"
  }
}