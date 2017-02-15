package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.StringBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer
import test_classes.block.Block
import test_classes.block.primitives.StringBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

class StringParser extends Parser[StringBlock] {

  def shouldParse(line: String): Boolean = line.matches("(val|var)[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*String[ ]*[=][ ]*\"[a-zA-Z][a-zA-Z0-9]*\"[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): StringBlock = {

    val declaration: Boolean = tokenizer.nextToken.token == "val" // "val" or "var"

    val name: String = tokenizer.nextToken.token

    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "String"
    tokenizer.nextToken // skip "="

    val value: String = tokenizer.nextToken.token

    new StringBlock(superBlock, declaration, name, value)
  }
}
