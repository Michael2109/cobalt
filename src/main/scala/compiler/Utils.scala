package compiler

import compiler.block.Block
import compiler.block.structures.methods.MethodBlock

object Utils {
  /* Returns the method a block is within */
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
}