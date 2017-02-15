package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.IntegerBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer
import test_classes.block.Block
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

class IntegerParser extends Parser[IntegerBlock] {

  // todo show how to set default values
  def shouldParse(line: String): Boolean = line.matches("(val|var)[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*int[ ]*[=][ ]*[0-9]+[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): IntegerBlock = {
    val declaration: Boolean = tokenizer.nextToken.token == "val" // "val" or "var"
    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "int"
    tokenizer.nextToken // skip "="
    val value: String = tokenizer.nextToken.token
    new IntegerBlock(superBlock, declaration,name, value)
  }
}
