package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.BooleanBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class BooleanParser extends Parser[BooleanBlock] {
  def shouldParse(line: String): Boolean = line.matches("var[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:boolean[ ]*[=][ ]*(true|false)")

  def parse(superBlock: Block, tokenizer: Tokenizer): BooleanBlock = {
    tokenizer.nextToken // skip "var"
    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "boolean"
    tokenizer.nextToken // skip "="
    val value: String = tokenizer.nextToken.token
    return new BooleanBlock(superBlock, name, value)
  }
}
