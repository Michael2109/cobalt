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

package compiler.structure.generators.loops

import compiler.structure.generators.GeneratorTest

object WhileGenTest extends GeneratorTest {

  def getOpeningCode(id: Int, pointer: String, value: String, byteCodeOp: String): String = {
    asm.newLabel("start" + id) +
      asm.visitLabel("start" + id) +
      asm.visitVarInsn("ILOAD", pointer) +
      asm.visitLdcInsn(value) +
      asm.newLabel("l" + id) +
      byteCodeOp
  }

  def getClosingCode(id: Int): String = {
    "mv.visitJumpInsn(GOTO, start" + id + ");\n" +
      "mv.visitLabel(l" + id + ");\n"
  }

}