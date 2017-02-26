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

package compiler.structure.generators.structures.methods

import compiler.structure.blocks.structures.methods.MethodBlock
import compiler.utilities.Utils

object MethodGen {

  def getOpeningCode(methodBlock: MethodBlock): String = {
    if (methodBlock.name != "main") {
      "   {\n" + "            /* Build '" + methodBlock.name + "' method */\n" + "            " +
        "MethodVisitor mv = cw.visitMethod(\n" + "                    " + methodBlock.modifier + " " + methodBlock.static + ",                         // public method\n" +
        "                    \"" + methodBlock.name + "\",                              // name\n" +
        "                    \"(" + methodBlock.parameterString + ")V\",                            // descriptor\n" +
        "                    null,                               // signature (null means not generic)\n" + "                    null);                              // exceptions (array of strings)\n" + "mv.visitCode();\n" + "\n" + "Label lMethod0 = new Label();\n" + "mv.visitLabel(lMethod0);\n"
    }
    else {
      "{\n" + "// Main Method\n" +
        "MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, \"main\", \"([Ljava/lang/String;)V\", null, null);\n" + "mv.visitCode();\n" + "Label lMethod0 = new Label();\n" + "mv.visitLabel(lMethod0);\n"
    }
  }

  def getClosingCode(methodBlock: MethodBlock): String = {
    if (methodBlock.name != "main") {
      "mv.visitInsn(RETURN);     \n" +
        "Label lMethod1 = new Label();\n" +
        "mv.visitLabel(lMethod1);\n" +
        "mv.visitLocalVariable(\"this\", \"L" + Utils.packageBlock(methodBlock).directory + "/" + methodBlock.name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n               " +
        "// Return integer from top of stack\n" +
        methodBlock.localVariableString +
        "  mv.visitMaxs(0, 0);\n" +
        "mv.visitEnd();\n" + "}\n"
    }
    else {
      "mv.visitInsn(RETURN);     \n" +
        "Label lMethod1 = new Label();\n" +
        "mv.visitLabel(lMethod1);\n" +
        "mv.visitLocalVariable(\"this\", \"L" + Utils.packageBlock(methodBlock).directory + "/" + methodBlock.name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n" +
        "mv.visitLocalVariable(\"args\", \"[Ljava/lang/String;\", null, lMethod0, lMethod1, 0);                " +
        "// Return integer from top of stack\n" + methodBlock.localVariableString +
        "  mv.visitMaxs(0, 0);\n" + "mv.visitEnd();\n" + "}\n"
    }
  }
}