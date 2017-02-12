package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.IntegerBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class IntegerParser extends Parser[IntegerBlock] {

  def shouldParse(line: String): Boolean = line.matches("var[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:int[ ]*[=][ ]*[0-9]+[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): IntegerBlock = {
    tokenizer.nextToken // skip "var"
    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "int"
    tokenizer.nextToken // skip "="
    val value: String = tokenizer.nextToken.token
    new IntegerBlock(superBlock, name, value)
  }
}
