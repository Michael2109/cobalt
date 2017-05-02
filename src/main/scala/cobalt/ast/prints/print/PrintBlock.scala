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

package cobalt.ast.prints.print

import cobalt.ast.{Block, Parsers}
import cobalt.symbol_table.SymbolTable
import cobalt.tokenizer.Tokenizer
import cobalt.utilities.Utils

/**
  * Represents a println
  *
  * @param superBlockInit
  * @param value
  * @param isVariableInit
  */
class PrintBlock(var superBlockInit: Block, var value: String, val isVariableInit: Boolean) extends Block(superBlockInit, false, false) {

  override def getName: String = ""

  override def getValue: String = value

  override def getType: String = "<PRINT>"

  override def getOpeningCode: String = {

    if (isVariableInit) {
      val row = SymbolTable.getInstance.getValue(Utils.getMethod(this).get, value)

      var v: Block = null
      for (p <- Parsers.parsers) {
        if (p.shouldParse(value)) {
          v = p.parse(superBlockInit, new Tokenizer(value))
        }
      }

      "mv.visitFieldInsn(GETSTATIC, \"java/lang/System\", \"out\", \"Ljava/io/PrintStream;\");\n" +
        v.getOpeningCode + "\n" +
        "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/io/PrintStream\", \"print\", \"(Ljava/lang/Object;)V\");"
    }
    else {
      "mv.visitFieldInsn(GETSTATIC, \"java/lang/System\", \"out\", \"Ljava/io/PrintStream;\");\n" +
        "mv.visitLdcInsn(" + value + ");\n" +
        "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/io/PrintStream\", \"print\", \"(Ljava/lang/String;)V\");"
    }
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "print: " + value + " " + stack
}