package compiler.parser.operators

import compiler.block.Block
import compiler.block.operators.SubtractBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class SubtractParser extends Parser[SubtractBlock] {
  def shouldParse(line: String): Boolean = line.matches("[a-zA-Z][a-zA-Z0-9]* [-][=] [0-9]+")

  def parse(superBlock: Block, tokenizer: Tokenizer): SubtractBlock = {
    val name: String = tokenizer.nextToken.getToken
    tokenizer.nextToken
    val value: String = tokenizer.nextToken.getToken
    return new SubtractBlock(superBlock, name, value)
  }
}
