package test_classes.parser.structures

import test_classes.block.Block
import test_classes.block.structures.MethodCallBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

/**
  * Calls a method inside a class
  */
class MethodCallParser extends Parser[MethodCallBlock] {

  def shouldParse(line: String): Boolean =  line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*\\(\\)[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): MethodCallBlock = {
    val name: String = tokenizer.nextToken.token // Get the string value of the next token.
    return new MethodCallBlock(superBlock, name, "method_call", null)
  }
}
