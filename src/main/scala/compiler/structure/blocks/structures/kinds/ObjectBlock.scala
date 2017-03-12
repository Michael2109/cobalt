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

package compiler.structure.blocks.structures.kinds

import compiler.data.parameters.Parameter
import compiler.structure.blocks.Block
import compiler.structure.blocks.packages.PackageBlock
import compiler.symbol_table.{Row, SymbolTable}

/**
  * Represents a class.
  * Creates a constructor method. Loops through all blocks unless it's a method or within a method adding to the constructor
  */
class ObjectBlock(var superBlockInit: Block, var name: String, var parameters: Array[Parameter], parentClass: String, implementedClasses: String) extends Block(superBlockInit, true, false) {

  SymbolTable.getInstance.addRow(new Row().setId(id).setName(getName).setType(getType).setValue(getValue).setMethodName("").setClassName(name))

  def getName: String = name

  def getValue: String = null

  def getType: String = "class"

  def getOpeningCode: String = {
      asm.getClassOpening(name) +
      asm.executeMethodOpening +
      asm.getClassWriter +
        asm.visitClassWriter("", packageBlock.directory + "/" + name, null, parentClass, null)
  }

  def packageBlock: PackageBlock = superBlock.subBlocks.find(_.isInstanceOf[PackageBlock]).getOrElse(new PackageBlock("")).asInstanceOf[PackageBlock]

  def getClosingCode: String = {
     " cw.visitEnd();\n" + "return cw.toByteArray();\n" +
       asm.getClosingBrace
    }

  override def toString: String = "object" + name

  // Called after file is parsed
  override def init(): Unit = {}
}