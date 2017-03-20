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

package compiler.structure.blocks.structures.methods

import compiler.data.parameters.Parameter
import compiler.structure.blocks.Block
import compiler.structure.blocks.modifiers.ModifierBlock
import compiler.structure.generators.structures.methods.MethodGen
import compiler.symbol_table.{Row, SymbolTable}
import compiler.utilities.Utils

class MethodBlock(var superBlockInit: Block, val name: String, val returnType: String, val isSealed: Boolean, var params: Array[Parameter]) extends Block(superBlockInit, true, false, false) {

  SymbolTable.getInstance.addRow(new Row().setId(id).setName(getName).setType(getType).setValue(getValue).setMethodName(name).setClassName(Utils.getClass(this).get.getName))

  val modifier : String = {

    // Check the modifier if it exists
    if (superBlock.isInstanceOf[ModifierBlock]) {
      if (superBlock.getValue == "private") "ACC_PRIVATE"
      else if (superBlock.getValue == "public") "ACC_PUBLIC"
      else if (superBlock.getValue == "protected") "ACC_PROTECTED"
      else "0"
    } else "0"
  }

  for (parameter <- params) {

    //    println("Adding to symbol table")
    SymbolTable.getInstance.addRow(new Row().setId(id).setName(parameter.getName).setType(parameter.getType).setMethodName(getName).setClassName(Utils.getClass(this).get.getName))

    Block.TOTAL_BLOCKS += 1
    localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getAsmType + "\", null, lMethod0, lMethod1, " + i + ");\n"
    // SymbolTable.getInstance.addRow(new Row().setMethodName(name).setId(i).setName(parameter.getName))

    i += 1
  }

  val `sealed`: String = if (isSealed) "+ACC_FINAL" else ""

  val parameterString: String = params.map(_.getAsmType).mkString("")

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
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    name + " ( " + paramString + ")"
  }
}