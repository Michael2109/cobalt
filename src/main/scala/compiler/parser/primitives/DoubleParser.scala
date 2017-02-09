package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.DoubleBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class DoubleParser extends Parser[DoubleBlock] {
  def shouldParse(line: String): Boolean = line.matches("var[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:double[ ]*[=][ ]*[0-9]+[.][0-9]*[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): DoubleBlock = {
    tokenizer.nextToken // skip "var"
    val name: String = tokenizer.nextToken.getToken
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "double"
    tokenizer.nextToken // skip "="
    var value: String = tokenizer.nextToken.getToken
    tokenizer.nextToken
    value += "." + tokenizer.nextToken.getToken
    return new DoubleBlock(superBlock, name, value)
  }
}
