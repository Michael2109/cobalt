package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.FloatBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class FloatParser extends Parser[FloatBlock] {
  def shouldParse(line: String): Boolean = line.matches("float [a-zA-Z][a-zA-Z0-9]* [=] [0-9]+[.][0-9]*f")

  def parse(superBlock: Block, tokenizer: Tokenizer): FloatBlock = {
    tokenizer.nextToken // skip float
    val name: String = tokenizer.nextToken.getToken
    tokenizer.nextToken
    var value: String = tokenizer.nextToken.getToken
    tokenizer.nextToken
    value += "." + tokenizer.nextToken.getToken
    return new FloatBlock(superBlock, name, value)
  }
}
