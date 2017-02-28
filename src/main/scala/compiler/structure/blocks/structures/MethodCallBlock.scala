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

package compiler.structure.blocks.structures

import compiler.data.parameters.Parameter
import compiler.structure.blocks.Block

/**
  * Calling of a method within the class
  */
class MethodCallBlock(var superBlockInit: Block, var name: String, var `type`: String, var params: Array[Parameter]) extends Block(superBlockInit, false, false) {

  def getParameters: Array[Parameter] = params

  def init() {}

  def getName: String = name

  def getValue: String = ""

  def getType: String = `type`

  def getOpeningCode: String = name + "();"

  def getClosingCode: String = ""

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return "method call: " + name + " ( " + paramString + ")"
  }
}