package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.FloatBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class FloatParser extends Parser[FloatBlock] {
  def shouldParse(line: String): Boolean = line.matches("var[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*float[ ]*[=][ ]*[0-9]+[.][0-9]*f[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): FloatBlock = {
    tokenizer.nextToken // skip "var"
    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "float"
    tokenizer.nextToken // skip "="
    var value: String = tokenizer.nextToken.token
    tokenizer.nextToken
    value += "." + tokenizer.nextToken.token
    return new FloatBlock(superBlock, name, value)
  }
}
