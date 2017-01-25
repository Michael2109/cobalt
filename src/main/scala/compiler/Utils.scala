package compiler

import compiler.block.Block
import compiler.block.structures.methods.MethodBlock

object Utils {
  /* Returns the method a block is within */
  def getMethod(block: Block): Block = {
    var result: Block = block
    while (!(result.isInstanceOf[MethodBlock])) {
      {
        if (block.getSuperBlock == null) {
          return null
        }
        result = result.getSuperBlock
        if (result == null)
          return result
      }
    }
    return result
  }
}