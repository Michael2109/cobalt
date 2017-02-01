package compiler.block

import java.util
import java.util.ArrayList
import java.util.Collections

import asm.ASMGenerator

/**
  *
  */
object Block {
  var TOTAL_BLOCKS: Int = 50
}

/**
  * Represents a block of code.
  *
  * @param superBlockInitTest The superBlock of this block
  * @param containerInit      Whether the block contains other blocks
  * @param variableInit       Whether the block is a variable
  */
abstract class Block(var superBlockInitTest: Block, val containerInit: Boolean, val variableInit: Boolean) {

  Block.TOTAL_BLOCKS += 1

  private val _subBlocks: java.util.ArrayList[Block] = new ArrayList[Block]
  private val _asm: ASMGenerator = new ASMGenerator;
  private var _id: Integer = Block.TOTAL_BLOCKS
  private var _superBlock: Block = superBlockInitTest
  private var _container: Boolean = containerInit
  private var _variable: Boolean = variableInit

  /* id GET and SET */
  def id = _id

  def id_=(value: Integer) = _id = value

  /* superBlock GET and SET */
  def superBlock = _superBlock

  def superBlock_=(value: Block) = _superBlock = value

  /* subBlocks GET */
  def subBlocks = _subBlocks.toArray(new Array[Block](_subBlocks.size()))

  def addBlock_=(value: Block) = _subBlocks.add(value)

  def removeBlock_=(value: Block) = _subBlocks.remove(value)

  // Called after file is parsed
  def init()

  /* Symbol table information */
  def getName: String

  def getValue: String

  def getType: String

  /* Bytecode for the opening and closing of the block */
  def getOpeningCode: String

  def getClosingCode: String

  /* Whether the block contains other blocks */
  def isContainer: Boolean = _container

  def container_=(value: Boolean) = _container = value

  /* Whether the block is  a variable */
  def isVariable: Boolean = _variable

  def variable_=(value: Boolean) = _variable = value

  def error(): Boolean = false

  def asm: ASMGenerator = _asm
}