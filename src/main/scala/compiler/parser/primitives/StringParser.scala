package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.StringBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class StringParser extends Parser[StringBlock] {

  def shouldParse(line: String): Boolean = line.matches("var[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:String[ ]*[=][ ]*\"[a-zA-Z][a-zA-Z0-9]*\"[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): StringBlock = {

    tokenizer.nextToken // skip "var"

    val name: String = tokenizer.nextToken.token

    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "String"
    tokenizer.nextToken // skip "="

    val value: String = tokenizer.nextToken.token

    return new StringBlock(superBlock, name, value)
  }
}
