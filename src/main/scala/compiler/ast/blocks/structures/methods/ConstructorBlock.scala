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

package compiler.ast.blocks.structures.methods

import compiler.ast.blocks.Block
import compiler.data.parameters.Parameter
import compiler.symbol_table.{Row, SymbolTable}
import compiler.utilities.Utils

/**
  * Represents a constructor
  *
  * @param superBlockInit
  * @param parameters
  * @param className
  */
class ConstructorBlock(val superBlockInit: Block, val parameters: List[Parameter], className: String) extends Block(superBlockInit, true, false) {

  SymbolTable.getInstance.addRow(new Row().setId(id).setName(getName).setType(getType).setValue(getValue).setMethodName("<init>").setClassName(className))

  val classBlock: Block = superBlock
  val parameterString = parameters.map(_.getType).mkString("")
  private val packageDir: String = Utils.getPackage(this)
  var localVariableString = ""
    var i = 1

  localVariableString += "mv.visitLocalVariable(\"this\", \"L" + packageDir + "/" + classBlock.getName + ";\", null, lConstructor0, lConstructor2, " + 0 + ");\n"
    for (parameter <- parameters) {
      // Add to the symbol table
      SymbolTable.getInstance.addRow(new Row().setId(id).setName(parameter.getName).setType(parameter.getType).setMethodName(getName).setClassName(className))


      Block.TOTAL_BLOCKS_$eq(Block.TOTAL_BLOCKS + 1)
      localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getType + "\", null, lConstructor0, lConstructor2, " + i + ");\n"
      //     SymbolTable.getInstance.addRow(new Row().setId(i).setName(parameter.getName).setClassName(className))
      i += 1

    }

  override def getName: String = "<init>"

  override def getValue: String = ""

  override def getType: String = "constructor"

  override def getOpeningCode: String = {

    asm.getOpeningBrace +
      asm.getMethodVisitor("<init>", "(" + parameterString + ")V", null, null) +
      asm.visitCode() +
      asm.getComment("Constructor") +
      asm.newLabel("lConstructor0") +
      asm.visitLabel("lConstructor0") +
      asm.visitVarInsn("ALOAD", "0") +
      "// Load \"this\" onto the stack\n" + "\n" +
      "mv.visitMethodInsn(INVOKESPECIAL," +
      "// Invoke an instance method (non-virtual)\n" +
      "\"java/lang/Object\", // Class on which the method is defined\n" +
      "\"<init>\"," +
      "// Name of the method\n" +
      "\"()V\"," +
      "// Descriptor\n" + "false);" +
      "// Is this class an interface?\n" + "\n" + "Label lConstructor2 = new Label();\n" + "mv.visitLabel(lConstructor2);\n" + "\n"
  }

  override def getClosingCode: String = {
    "mv.visitInsn(RETURN);                     " + localVariableString + " // End the constructor method\n" + "mv.visitMaxs(0, 0);\n" + "mv.visitEnd();\n" + "}"
  }

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- parameters) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    "constructor: ( " + paramString + ")"
  }
}