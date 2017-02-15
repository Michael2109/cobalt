package test_classes

import test_classes.block.Block
import test_classes.block.structures.FileBlock
import test_classes.block.structures.methods.MethodBlock

object Utils {

  /**
    * Returns the method a block is within
    *
    * @param block
    * @return
    */
  def getMethod(block: Block): Block = {
    var result: Block = block
    while (!(result.isInstanceOf[MethodBlock])) {
      {
        if (block.superBlock == null) {
          return null
        }
        result = result.superBlock
        if (result == null)
          return result
      }
    }
    return result
  }

  def getFileBlock(blockInit: Block) : Block = {

    val fileBlock:Block = {
      var block: Block = blockInit
      while(!block.isInstanceOf[FileBlock]){
        block = block.superBlock
      }
      block
    }
    fileBlock
  }

  /**
    * Returns whether a int value, string value, or variable reference
    * int value = 0
    * string value = 1
    * variable ref = 2
    *
    * @param name
    * @return
    */
  def getType(name: String): Int = {

    if (name.charAt(0).isDigit) return 0

    if (name.startsWith("\"")) return 1

    return 2
  }

  /**
    * Returns the indentation of the block
    * @param line
    * @return
    */
  def getIndentation(line: String): Int = {
    var amount: Int = 0
    var indentation: Int = 0
    for (character <- line.toCharArray) {
      if (character != ' ') return indentation
      else {
        amount += 1
        if (amount % 4 == 0) indentation += 1
      }
    }
    indentation
  }

  /**
    * Prints block information
    * @param block
    * @param indentation
    */
  def printBlockInfo(block: Block, indentation: Int = 0) {
    var indentationString: String = ""
    var i: Int = 0
    while (i < indentation) {
      {
        indentationString += "    "
      }
      {
        i += 1
        i - 1
      }
    }
    System.out.println(indentationString + block.toString)
    for (sub <- block.subBlocks) {
      printBlockInfo(sub, indentation + 1)
    }
  }

}