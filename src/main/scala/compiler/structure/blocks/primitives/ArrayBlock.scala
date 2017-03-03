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

package compiler.structure.blocks.primitives

import compiler.structure.blocks.Block

class ArrayBlock( superBlockInit: Block, declaration : Boolean, name: String,  value: String) extends Block(superBlockInit, false, true) {

  override def init() {}

  override def getName: String = name

  override def getValue: String = value

  override def getType(): String = "array"


  override  def getOpeningCode: String = {
    return "array " + name + " = '" + value + "';"
  }

  override def getClosingCode: String = {
    return ""
  }

  override def toString: String = "array: " + name + " = " + value

}