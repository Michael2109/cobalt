package compiler.parser.operators

import compiler.block.Block
import compiler.block.operators.MultiplyBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer
import test_classes.block.Block
import test_classes.block.operators.MultiplyBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

class MultiplyParser extends Parser[MultiplyBlock] {

  def shouldParse(line: String): Boolean = line.matches("[a-zA-Z][a-zA-Z0-9]* [*][=] [0-9]+")

  def parse(superBlock: Block, tokenizer: Tokenizer): MultiplyBlock = {
    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken
    val value: String = tokenizer.nextToken.token
    return new MultiplyBlock(superBlock, name, value)
  }
}
