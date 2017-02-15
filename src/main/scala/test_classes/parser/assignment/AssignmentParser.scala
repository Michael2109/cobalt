package test_classes.parser.primitives

import test_classes.block.Block
import test_classes.block.assignment.AssignmentBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

class AssignmentParser extends Parser[AssignmentBlock] {

  // todo show how to set default values
  def shouldParse(line: String): Boolean = line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*[=][ ]*[.]+[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): AssignmentBlock = {
    // get the id of the variable


    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "int"
    tokenizer.nextToken // skip "="
    val value: String = tokenizer.nextToken.token
    new AssignmentBlock(superBlock,true,name, value)
  }
}
