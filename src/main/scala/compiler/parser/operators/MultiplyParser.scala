package compiler.parser.operators

import compiler.block.Block
import compiler.block.operators.MultiplyBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class MultiplyParser extends Parser[MultiplyBlock] {

  def shouldParse(line: String): Boolean = line.matches("[a-zA-Z][a-zA-Z0-9]* [*][=] [0-9]+")

  def parse(superBlock: Block, tokenizer: Tokenizer): MultiplyBlock = {
    val name: String = tokenizer.nextToken.getToken
    tokenizer.nextToken
    val value: String = tokenizer.nextToken.getToken
    return new MultiplyBlock(superBlock, name, value)
  }
}
