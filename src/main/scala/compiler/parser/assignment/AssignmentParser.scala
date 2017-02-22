package compiler.parser.assignment

import compiler.block.Block
import compiler.block.assignment.AssignmentBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class AssignmentParser extends Parser[AssignmentBlock] {

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
