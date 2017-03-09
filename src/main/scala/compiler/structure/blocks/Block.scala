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

package compiler.structure.blocks

import compiler.utilities.ASMGenerator
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ListBuffer

/**
  * Stores the total amount of blocks to use as a unique identifier
  */
object Block {
  var TOTAL_BLOCKS: Int = 50
}

/**
  * Represents a blocks of code.
  *
  * @param superBlockInitTest The superBlock of this blocks
  * @param containerInit      Whether the blocks contains other blocks
  * @param variableInit       Whether the blocks is a variable
  */
abstract case class Block(var superBlockInitTest: Block, val containerInit: Boolean, val variableInit: Boolean, immutableInit: Boolean = false) {

  Block.TOTAL_BLOCKS += 1

  val immutable: Boolean = immutableInit
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  val expressions: ListBuffer[Block] = ListBuffer[Block]()
  private val _subBlocks: ListBuffer[Block] = new ListBuffer[Block]
  private val _asm: ASMGenerator = new ASMGenerator;
  private var _id: Integer = Block.TOTAL_BLOCKS
  private var _superBlock: Block = superBlockInitTest
  private var _container: Boolean = containerInit
  private var _variable: Boolean = variableInit

  /* id GET and SET */
  def id = _id

  def id_=(value: Integer) = _id = value

  /* subBlocks GET */
  def subBlocks = _subBlocks

  def addBlock_=(value: Block) = {
    _subBlocks.append(value)
    value.superBlock = this
  }

  /* superBlock GET and SET */
  def superBlock = _superBlock

  def superBlock_=(value: Block) = _superBlock = value

  def addBlocks_=(value: ListBuffer[Block]) = _subBlocks.appendAll(value)

  def removeBlock_=(value: Block) = _subBlocks.filter(_ != value)

  // Called after file is parsed
  def init()

  /* Symbol table information */
  def getName: String

  def getValue: String

  def getType(): String

  /* Bytecode for the opening and closing of the blocks */
  def getOpeningCode: String

  def getClosingCode: String

  /* Whether the blocks contains other blocks */
  def isContainer: Boolean = _container

  def container_=(value: Boolean) = _container = value

  /* Whether the blocks is  a variable */
  def isVariable: Boolean = _variable

  def variable_=(value: Boolean) = _variable = value


  // Error check on the line of code
  def error(): Boolean = false

  // Used to generate ASM code
  def asm: ASMGenerator = _asm
}