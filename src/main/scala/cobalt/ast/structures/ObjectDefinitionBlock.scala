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

package cobalt.ast.structures

import cobalt.ast.Block
import cobalt.ast.structures.kinds.{ClassBlock, ObjectBlock}
import cobalt.utilities.Utils


/**
  * Represents pushing an object onto the stack
  *
  * @param superBlockInit
  * @param newKeyword
  * @param initClassName
  */
class ObjectDefinitionBlock(superBlockInit: Block, newKeyword: String, initClassName: String, argumentBlocks: List[Block]) extends Block(superBlockInit, false, true, false) {

  private val argTypeString: String = argumentBlocks.map(b => Utils.getASMType(b.getType)).mkString("")
  private val argStackString: String = argumentBlocks.map(_.getOpeningCode).mkString("\n")

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

  override val getName: String = ""

  override val getValue: String = ""

  override val getType: String = initClassName

  override def getOpeningCode: String = {

    val directory = if (Utils.getDirectory(this, initClassName) == "") Utils.getPackage(this).replace(".", "/") else Utils.getDirectory(this, initClassName)

    "mv.visitTypeInsn(NEW, \"" + directory + "/" + initClassName + "\");\n" + "mv.visitInsn(DUP);\n" +
      argStackString +
      "mv.visitMethodInsn(INVOKESPECIAL, \"" + directory + "/" + initClassName + "\", \"<init>\", \"(" + argTypeString + ")V\", false);\n"
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "new " + initClassName + "()"

}