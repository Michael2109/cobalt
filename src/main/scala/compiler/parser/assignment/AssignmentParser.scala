package compiler.parser.primitives

import compiler.block.Block
import compiler.block.assignment.AssignmentBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class AssignmentParser extends Parser[AssignmentBlock] {

  // todo show how to set default values
  def shouldParse(line: String): Boolean = line.matches("(val|var)[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*int[ ]*[=][ ]*[0-9]+[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): AssignmentBlock = {
    // get the id of the variable


    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "int"
    tokenizer.nextToken // skip "="
    val value: String = tokenizer.nextToken.token
    new AssignmentBlock(superBlock, declaration,name, value)
  }
}
