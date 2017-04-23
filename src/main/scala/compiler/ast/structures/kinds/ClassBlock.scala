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

package compiler.ast.structures.kinds

import compiler.ast.Block
import compiler.ast.modifiers.ModifierBlock
import compiler.ast.packages.PackageBlock
import compiler.ast.structures.methods.{ConstructorBlock, MethodBlock}
import compiler.data.parameters.Parameter
import compiler.symbol_table.{Row, SymbolTable}
import compiler.tokenizer.Token
import compiler.tokenizer.tokens.keywords.modifiers._
import compiler.utilities.Utils

/**
  * Represents a class.
  * Creates a constructor method. Loops through all blocks unless it's a method or within a method adding to the constructor
  */
class ClassBlock(var superBlockInit: Block, modifierTokens: List[Token], name: String, parameters: List[Parameter], extendsTokens: List[Token], implementedTokens: List[Token]) extends Block(superBlockInit, true, false) {

  SymbolTable.getInstance.addRow(new Row().setId(id).setName(getName).setType(getType).setValue(getValue).setMethodName("").setClassName(name))

  val ref = this

  val `sealed`: String = if (false) "+ACC_FINAL" else ""

  private val modifiersASM = {
    var result = ""
    if (!modifierTokens.exists(m => m.isInstanceOf[InternalToken])) {
      result = "+ACC_PRIVATE"
    }
    for (m <- modifierTokens) {
      m match {
        case _: PublicToken => result = "+ACC_PUBLIC"
        case _: InternalToken => result = "0"
        case _: ProtectedToken => result = "+ACC_PROTECTED"
        case _: AbstractToken => result += "+ACC_ABSTRACT"
        case _ =>
      }
    }

    result
  }

  // Parameters added to constuctor
  private var parameterString: String = parameters.map(_.getType).mkString("")

  // Local variables from the parameters
  private var localVariableString: String = ""

  // Create a constructor blocks and add it to the class blocks
  private var constructorBlock: Block = new ConstructorBlock(this, parameters, name)

  // Add the constructor block to the class
  addBlock_=(constructorBlock)

  // Move anything outside a method and within the class to a constructor blocks
  for (sub <- subBlocks) {
    moveToConstructor(sub)
  }

  for (parameter <- parameters) {
    Block.TOTAL_BLOCKS_$eq(Block.TOTAL_BLOCKS + 1)
    localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getType + "\", null, lConstructor0, lConstructor2, " + Block.TOTAL_BLOCKS + ");\n"
  }


  /**
    * Moves all blocks that are inside the class and outside methods into the constructor blocks
    *
    * @param block
    */
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


  /**
    * Gets the package blocks
    *
    * @return
    */
  def packageBlock: PackageBlock = {
    superBlock.subBlocks.find(_.isInstanceOf[PackageBlock]).getOrElse(new PackageBlock("")).asInstanceOf[PackageBlock]
  }

  override def getName: String = name

  override def getValue: String = null

  override def getType: String = "<CLASS>"

  override def getOpeningCode: String = {
    "public class " + name + "{\n" +
      "public static byte[] execute() throws Exception {\n" +
      "ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);\n" +
      "FieldVisitor fv;\n" +
      "cw.visit(V1_7, " + modifiersASM + ", \"" + packageBlock.directory + "/" + name + "\", " + null + ", \"" + (if (extendsTokens.size == 0) "java/lang/Object" else (extendsTokens.map(t => Utils.getDirectory(ref, t.token) + "/" + t.token).mkString(""))) + "\", new String[]{});\n"
  }

  override def getClosingCode: String = {
    "cw.visitEnd();\n" +
      "return cw.toByteArray();\n" +
      "}"
  }

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- parameters) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    name + " ( " + paramString + ") <EXTENDS> " + extendsTokens + " <IMPLEMENTS> " + implementedTokens + " " + stack + " <MODIFIERS>" + modifierTokens
  }
}