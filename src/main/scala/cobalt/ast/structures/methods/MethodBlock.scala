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

package cobalt.ast.structures.methods

import cobalt.ast.Block
import cobalt.ast.modifiers.ModifierBlock
import cobalt.symbol_table.{Row, SymbolTable}
import cobalt.tokenizer.tokens.keywords.modifiers._
import cobalt.utilities.Utils

class MethodBlock(var superBlockInit: Block, modifierTokens: List[ModifierToken], name: String, returnType: String, isSealed: Boolean, paramBlocks: List[Block]) extends Block(superBlockInit, true, false, false) {


  SymbolTable.getInstance.addRow(new Row().setId(id).setName(getName).setType(getType).setValue(getValue).setMethodName(name).setClassName(Utils.getClass(this).get.getName))

  paramBlocks.zipWithIndex.map { case (element, index) =>
    SymbolTable.getInstance.addRow(new Row().setId(index+1).setName(element.getName).setType(element.getType).setValue("").setMethodName(name).setClassName(Utils.getClass(this).get.getName))
  }

  val modifiersASM = {

    // Check the modifier if it exists
    if (superBlock.isInstanceOf[ModifierBlock]) {
      if (superBlock.getValue == "private") "ACC_PRIVATE"
      else if (superBlock.getValue == "public") "ACC_PUBLIC"
      else if (superBlock.getValue == "protected") "ACC_PROTECTED"
      else "0"
    } else {

      var result = ""
      if (!(modifierTokens.find(m => m.isInstanceOf[InternalToken]).size > 0)) {
        result = "+ACC_PRIVATE"
      }
      for (m <- modifierTokens) {
        if (m.isInstanceOf[PublicToken]) {
          result = "+ACC_PUBLIC"
        }
        else if (m.isInstanceOf[InternalToken]) {
          result = "0"
        }
        else if (m.isInstanceOf[ProtectedToken]) {
          result = "+ACC_PROTECTED"
        }
        else if (m.isInstanceOf[AbstractToken]) {
          result += "+ACC_ABSTRACT"
        }
      }
      result
    }
  }

  val `sealed`: String = if (isSealed) "+ACC_FINAL" else ""
  val parameterString: String = paramBlocks.map(b => Utils.getWrapperType(b.getType)).mkString("")

  var localVariableString: String = paramBlocks.zipWithIndex.map { case (element, index) =>
    "mv.visitLocalVariable(\"" + element.getName + "\", \"" + Utils.getWrapperType(element.getType) + "\", null, lMethod0, lMethod1, " + (index + 1) + ");".mkString("\n")
  }.mkString("")

  var i = 1

  def getName: String = name

  def getType: String = returnType

  def getValue: String = ""

  def static: String = if (!Utils.isClass(this)) "+ACC_STATIC" else ""

  def getOpeningCode: String = {
    if (getName != "main") {
      "   {\n" + "            /* Build '" + getName + "' method */\n" + "            " +
        "MethodVisitor mv = cw.visitMethod(\n" + "                    " + modifiersASM + " " + static + " " + `sealed` + ",                         // public method\n" +
        "                    \"" + getName + "\",                              // name\n" +
        "                    \"(" + parameterString + ")" + (if (returnType == "void") "V" else ("L" + Utils.getDirectory(this, returnType)).trim + "/" + returnType.trim + ";") + "\",                            // descriptor\n" +
        "                    " + "null" + ",                               // signature (null means not generic)\n" +
        "                    null);                              // exceptions (array of strings)\n" + "mv.visitCode();\n" + "\n" + "Label lMethod0 = new Label();\n" + "mv.visitLabel(lMethod0);\n"
    }
    else {
      "{\n" + "// Main Method\n" +
        "MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, \"main\", \"([Ljava/lang/String;)V\", null, null);\n" + "mv.visitCode();\n" + "Label lMethod0 = new Label();\n" + "mv.visitLabel(lMethod0);\n"
    }
  }


  def getClosingCode: String = {
    if (getName != "main") {
      "mv.visitInsn(RETURN);     \n" +
        "Label lMethod1 = new Label();\n" +
        "mv.visitLabel(lMethod1);\n" +
        "mv.visitLocalVariable(\"this\", \"L" + Utils.packageBlock(this).directory + "/" + getName + ";\", null, lMethod0, lMethod1, " + 0 + ");\n" +
        "// Return integer from top of stack\n" +
        localVariableString +
        "mv.visitMaxs(0, 0);\n" +
        "mv.visitEnd();\n" + "}\n"
    }
    else {
      "mv.visitInsn(RETURN);     \n" +
        "Label lMethod1 = new Label();\n" +
        "mv.visitLabel(lMethod1);\n" +
        "mv.visitLocalVariable(\"this\", \"L" + Utils.packageBlock(this).directory + "/" + getName + ";\", null, lMethod0, lMethod1, " + 0 + ");\n" +
        "mv.visitLocalVariable(\"args\", \"[Ljava/lang/String;\", null, lMethod0, lMethod1, 0);                " +
        "// Return integer from top of stack\n" + localVariableString +
        "  mv.visitMaxs(0, 0);\n" + "mv.visitEnd();\n" + "}\n"
    }
  }


  override def toString: String = {
    var paramString: String = "modifierTokens: " + modifierTokens + " "
    for (parameter <- paramBlocks) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    name + " ( " + paramString + ")"
  }
}