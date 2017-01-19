package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.BooleanBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class BooleanParser extends Parser[BooleanBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("boolean[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*[=][ ]*(true|false)")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): BooleanBlock = {
    tokenizer.nextToken // skip boolean
    val name: String = tokenizer.nextToken.getToken
    tokenizer.nextToken // =
    val value: String = tokenizer.nextToken.getToken
    return new BooleanBlock(superBlock, name, value)
  }
}