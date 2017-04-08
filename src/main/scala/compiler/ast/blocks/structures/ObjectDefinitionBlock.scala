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
import compiler.ast.blocks.imports.ImportBlock
import compiler.ast.blocks.structures.kinds.{ClassBlock, ObjectBlock}
import compiler.utilities.Utils


/**
  * Represents pushing an object onto the stack
  *
  * @param superBlockInit
  * @param newKeyword
  * @param initClassName
  */
class ObjectDefinitionBlock(superBlockInit: Block, newKeyword: String, initClassName: String) extends Block(superBlockInit, false, true, false) {

  private val parameterString: String = ""
  private val argumentString: String = ""
  private val directory: String = {
    var result = ""
    for (i <- Utils.getFileBlock(this).subBlocks) {

      if (i.isInstanceOf[ImportBlock] && i.asInstanceOf[ImportBlock].fileName == initClassName) {
        result = i.asInstanceOf[ImportBlock].directory
      }
    }
    result
  }


  // Returns the main class name for the file
  def getObjectName: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!block.isInstanceOf[ClassBlock] && !block.isInstanceOf[ObjectBlock]) {
        block = block.superBlock
    }
    // Get the directory of the Object
    block.getName
  }

  override def getName: String = ""

  override def getValue: String = ""

  override def getType: String = initClassName

  override def getOpeningCode: String = {
    "mv.visitTypeInsn(NEW, \"" + directory + (if (directory == "") ""
    else "/") + initClassName + "\");\n" + "mv.visitInsn(DUP);\n" + argumentString + "mv.visitMethodInsn(INVOKESPECIAL, \"" + directory + (if (directory == "") ""
    else "/") + initClassName + "\", \"<init>\", \"(" + parameterString + ")V\", false);\n"
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "new " + initClassName + "()"

}