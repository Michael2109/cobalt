package compiler.parser.structures

import compiler.block.Block
import compiler.block.structures.objects.MethodCallBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

/**
  * Calls a method inside a class
  */
class MethodCallParser extends Parser[MethodCallBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*\\(\\)[ ]*")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): MethodCallBlock = {
    val name: String = tokenizer.nextToken.getToken // Get the string value of the next token.
    return new MethodCallBlock(superBlock, name, "method_call", null)
  }
}