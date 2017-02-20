package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.FloatBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class FloatParser extends Parser[FloatBlock] {
  def shouldParse(line: String): Boolean = line.matches("(val|var)[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*float[ ]*[=][ ]*[0-9]+[.][0-9]*f[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): FloatBlock = {
    val declaration: Boolean = tokenizer.nextToken.token == "val" // "val" or "var"
    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "float"
    tokenizer.nextToken // skip "="
    var value: String = tokenizer.nextToken.token
    tokenizer.nextToken
    value += "." + tokenizer.nextToken.token
    new FloatBlock(superBlock, declaration,name, value)
  }
}
