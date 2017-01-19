package compiler.parser.operators

import compiler.block.Block
import compiler.block.operators.DivideBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class DivideParser extends Parser[DivideBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("[a-zA-Z][a-zA-Z0-9]* [/][=] [0-9]+")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): DivideBlock = {
    val name: String = tokenizer.nextToken.getToken
    tokenizer.nextToken.getToken
    val value: String = tokenizer.nextToken.getToken
    return new DivideBlock(superBlock, name, value)
  }
}