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

package compiler.block.structures.methods

import compiler.block.Block
import compiler.block.modifiers.ModifierBlock
import compiler.generators.structures.methods.MethodGen
import compiler.structure.parameters.Parameter
import compiler.symbol_table.{Row, SymbolTable}
import compiler.utilities.Utils

class MethodBlock(var superBlockInit: Block, var name: String, var `type`: String, var params: Array[Parameter]) extends Block(superBlockInit, true, false,false) {

  val modifier : String = {
    // Check the modifier if it exists
    if(superBlock.isInstanceOf[ModifierBlock]){
      if(superBlock.getValue == "private"){
        "ACC_PRIVATE"
      }else  if(superBlock.getValue == "public"){
        "ACC_PUBLIC"
      }else if(superBlock.getValue == "protected"){
        "ACC_PROTECTED"
      }else {
        "0"
      }
    }else{
      "0"
    }
  }

  val parameterString: String = params.map(_.getAsmType).mkString("")

  var localVariableString: String = ""

  def getName: String = name

  def getType: String = `type`

  def getValue: String = ""

  def init() {

    var i = 1
    for (parameter <- params) {
      Block.TOTAL_BLOCKS += 1
      localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getAsmType + "\", null, lMethod0, lMethod1, " + i + ");\n"
      SymbolTable.getInstance.addRow(new Row().setMethodName(name).setId(i).setName(parameter.getName))

      i += 1
    }

  }



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
    return name + " ( " + paramString + ")"
  }
}