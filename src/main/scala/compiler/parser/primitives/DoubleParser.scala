package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.DoubleBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer
import test_classes.block.Block
import test_classes.block.primitives.DoubleBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

class DoubleParser extends Parser[DoubleBlock] {
  def shouldParse(line: String): Boolean = line.matches("(val|var)[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*double[ ]*[=][ ]*[0-9]+[.][0-9]*[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): DoubleBlock = {
    val declaration: Boolean = tokenizer.nextToken.token == "val" // "val" or "var"
    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "double"
    tokenizer.nextToken // skip "="
    var value: String = tokenizer.nextToken.token
    tokenizer.nextToken
    value += "." + tokenizer.nextToken.token
    new DoubleBlock(superBlock, declaration, name, value)
  }
}
