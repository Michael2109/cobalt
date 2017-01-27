package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.IntegerBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class IntegerParser extends Parser[IntegerBlock] {

  def shouldParse(line: String): Boolean = line.matches("int[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]+[=][ ]+[0-9]+")

  def parse(superBlock: Block, tokenizer: Tokenizer): IntegerBlock = {
    tokenizer.nextToken // skip int
    val name: String = tokenizer.nextToken.getToken
    tokenizer.nextToken
    val value: String = tokenizer.nextToken.getToken
    return new IntegerBlock(superBlock, name, value)
  }
}
