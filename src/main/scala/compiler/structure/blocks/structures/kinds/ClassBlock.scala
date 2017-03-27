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
import compiler.structure.blocks.modifiers.ModifierBlock
import compiler.structure.blocks.packages.PackageBlock
import compiler.structure.blocks.structures.methods.{ConstructorBlock, MethodBlock}
import compiler.symbol_table.{Row, SymbolTable}
import compiler.tokenizer.TokenType

/**
  * Represents a class.
  * Creates a constructor method. Loops through all blocks unless it's a method or within a method adding to the constructor
  */
class ClassBlock(var superBlockInit: Block, modifiers: List[TokenType], var name: String, var parameters: Array[Parameter], parentClass: String, implementedClasses: String) extends Block(superBlockInit, true, false) {

  SymbolTable.getInstance.addRow(new Row().setId(id).setName(getName).setType(getType).setValue(getValue).setMethodName("").setClassName(name))

  val `sealed`: String = if (false) "+ACC_FINAL" else ""
  // Parameters added to constuctor
  private var parameterString: String = ""
  // Local variables from the parameters
  private var localVariableString: String = ""

  // Create a constructor blocks and add it to the class blocks
  private var constructorBlock: Block = new ConstructorBlock(this, parameters, name)

  addBlock_=(constructorBlock)

  def getName: String = name

  def getValue: String = null

  def getType: String = "class"

  /**
    * Performed just before compiling blocks to allow for action when all blocks parsed
    */
  def init() {
    // Move anything outside a method and within the class to a constructor blocks
    for (sub <- subBlocks) {
      moveToConstructor(sub)
    }

    for (parameter <- parameters) {
      parameterString += parameter.getAsmType
      Block.TOTAL_BLOCKS_$eq(Block.TOTAL_BLOCKS + 1)
      localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getAsmType + "\", null, lConstructor0, lConstructor2, " + Block.TOTAL_BLOCKS + ");\n"
    }
  }

  // Moves all blocks that are inside the class and outside methods into the constructor blocks
  def moveToConstructor(block: Block) {
    if (block.isInstanceOf[MethodBlock] || block.isInstanceOf[ConstructorBlock] || block.isInstanceOf[ModifierBlock]) {
      return
    }
    else {
      val ref: Block = block
      constructorBlock.addBlock_=(ref)
      block.superBlock.removeBlock_=(block)
      block.superBlock_=(constructorBlock)
    }
    for (sub <- block.subBlocks) {
      moveToConstructor(sub)
    }
  }

  def getOpeningCode: String = {
      asm.getClassOpening(name) +
      asm.executeMethodOpening +
      asm.getClassWriter +
        asm.visitClassWriter(`sealed`, packageBlock.directory + "/" + name, null, parentClass, null)
  }

  /**
    * Gets the package blocks
    *
    * @return
    */
  def packageBlock: PackageBlock = {
    superBlock.subBlocks.find(_.isInstanceOf[PackageBlock]).getOrElse(new PackageBlock("")).asInstanceOf[PackageBlock]
  }

  def getClosingCode: String = {
     "cw.visitEnd();\n" +
       "return cw.toByteArray();\n" +
       asm.getClosingBrace
     }


  override def toString: String = {
    var paramString: String = ""
    for (parameter <- parameters) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    name + " ( " + paramString + ") extends " + parentClass + " implements " + implementedClasses + " " + expressions + " modifiers:" + modifiers
  }
}