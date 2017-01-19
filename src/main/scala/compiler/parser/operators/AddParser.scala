package compiler.parser.operators

import compiler.block.Block
import compiler.block.operators.AddBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class AddParser extends Parser[AddBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*[+][=][ ]*[0-9]+")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): AddBlock = {
    val name: String = tokenizer.nextToken.getToken
    tokenizer.nextToken.getToken
    val value: String = tokenizer.nextToken.getToken
    return new AddBlock(superBlock, name, value)
  }
}