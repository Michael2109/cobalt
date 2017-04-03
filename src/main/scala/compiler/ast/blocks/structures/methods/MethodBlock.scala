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
import compiler.ast.blocks.modifiers.ModifierBlock
import compiler.ast.generators.structures.methods.MethodGen
import compiler.data.parameters.Parameter
import compiler.symbol_table.{Row, SymbolTable}
import compiler.tokenizer.tokens.keywords.modifiers._
import compiler.utilities.Utils

class MethodBlock(var superBlockInit: Block, val modifierTokens: List[ModifierToken], name: String, val returnType: String, val isSealed: Boolean, var params: Array[Parameter]) extends Block(superBlockInit, true, false, false) {

  println(superBlockInit)

  SymbolTable.getInstance.addRow(new Row().setId(id).setName(getName).setType(getType).setValue(getValue).setMethodName(name).setClassName(Utils.getClass(this).get.getName))


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

  for (parameter <- params) {

    //    println("Adding to symbol table")
    SymbolTable.getInstance.addRow(new Row().setId(id).setName(parameter.getName).setType(parameter.getType).setMethodName(getName).setClassName(Utils.getClass(this).get.getName))

    Block.TOTAL_BLOCKS += 1
    localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getType + "\", null, lMethod0, lMethod1, " + i + ");\n"
    // SymbolTable.getInstance.addRow(new Row().setMethodName(name).setId(i).setName(parameter.getName))

    i += 1
  }

  val `sealed`: String = if (isSealed) "+ACC_FINAL" else ""
  val parameterString: String = params.map(_.getType).mkString("")

  var localVariableString: String = ""
  var i = 1

  def getName: String = name

  def getType: String = returnType

  def getValue: String = ""



  def static: String = if(!Utils.isClass(this))"+ACC_STATIC" else ""

  def getOpeningCode: String = {
    MethodGen.getOpeningCode(this)
  }


  def getClosingCode: String = {
    MethodGen.getClosingCode(this)
  }


  override def toString: String = {
    var paramString: String = "modifierTokens: " + modifierTokens + " "
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    name + " ( " + paramString + ")"
  }
}