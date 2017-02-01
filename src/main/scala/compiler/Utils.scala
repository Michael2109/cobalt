package compiler

import compiler.block.Block
import compiler.block.structures.methods.MethodBlock
import compiler.tokenizer.TokenType.TokenType
import compiler.tokenizer.Tokenizer

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

}