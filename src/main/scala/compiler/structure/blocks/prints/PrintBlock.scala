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

package compiler.structure.blocks.prints

import compiler.structure.blocks.Block
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils
class PrintBlock(var superBlockInit: Block, var value: String, val isVariableInit: Boolean) extends Block(superBlockInit, false, false) {

  def init() {
  }

  def getName: String = null

  def getValue: String = value

  def getType: String = "print"

  def getOpeningCode: String = {
    if (isVariableInit) {
      return "mv.visitFieldInsn(GETSTATIC, \"java/lang/System\", \"out\", \"Ljava/io/PrintStream;\");\n" +
        "mv.visitVarInsn(ALOAD, " + SymbolTable.getInstance.getValue(Utils.getMethod(this), value).getId + ");" +
        "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/io/PrintStream\", \"println\", \"(Ljava/lang/String;)V\");"
    }
    else {
      //return "System.out.println(\""+value+"\");";
      return "mv.visitFieldInsn(GETSTATIC, \"java/lang/System\", \"out\", \"Ljava/io/PrintStream;\");\n" +
        "mv.visitLdcInsn(\"" + value + "\");\n" +
        "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/io/PrintStream\", \"println\", \"(Ljava/lang/String;)V\");"
    }
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "print: " + value
}