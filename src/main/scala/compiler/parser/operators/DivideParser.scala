package compiler.parser.operators

import compiler.block.Block
import compiler.block.operators.DivideBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class DivideParser extends Parser[DivideBlock] {
  def shouldParse(line: String): Boolean = line.matches("[a-zA-Z][a-zA-Z0-9]* [/][=] [0-9]+")

  def parse(superBlock: Block, tokenizer: Tokenizer): DivideBlock = {
    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken
    val value: String = tokenizer.nextToken.token
    return new DivideBlock(superBlock, name, value)
  }
}
