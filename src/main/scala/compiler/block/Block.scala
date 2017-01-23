package compiler.block

import java.util.ArrayList
import java.util.Collections

/**
  * Represents a block of code.
  */
object Block {
  var TOTAL_BLOCKS: Int = 0
}

abstract class Block(var superBlockInit: Block, val containerInit: Boolean, val variableInit: Boolean) {

  protected var subBlocks: ArrayList[Block] = new ArrayList[Block]
  // true ifs block can store sub blocks

  // Block ID used for reference when storing in local variable table
  private var id: Integer = {
    Block.TOTAL_BLOCKS += 1
    Block.TOTAL_BLOCKS - 1
  }
  private var superBlock: Block = superBlockInit
  private var container: Boolean = containerInit
  private var variable: Boolean = variableInit

  def getBlockTree: ArrayList[Block] = {
    val blocks: ArrayList[Block] = new ArrayList[Block]
    var block: Block = this
    do {
      {
        {
          blocks.add(block)
          block = block.getSuperBlock
        }
      }
    } while (block != null)
    Collections.reverse(blocks)
    return blocks
  }

  def getSuperBlock: Block = {
    return superBlock
  }

  def setSuperBlock(superBlock: Block) {
    this.superBlock = superBlock
  }

  def getSubBlocks: Array[Block] = {
    return subBlocks.toArray(new Array[Block](subBlocks.size))
  }

  def addBlock(block: Block) {
    subBlocks.add(block)
  }

  // Removes a block
  def removeBlock(block: Block) {
    subBlocks.remove(block)
  }

  // Called before looping through blocks to generate code. Allows for method to be called when all blocks are loaded
  def init()

  def getName: String

  def getValue: String

  def getType: String

  def getOpeningCode: String

  def getBodyCode: String

  def getClosingCode: String

  def getId: Integer = {
    return id
  }

  def setId(id: Integer) {
    this.id = id
  }

  def isContainer: Boolean = {
    return container
  }

  def setContainer(container: Boolean) {
    this.container = container
  }

  def isVariable: Boolean = {
    return variable
  }

  def setVariable(variable: Boolean) {
    this.variable = variable
  }
}