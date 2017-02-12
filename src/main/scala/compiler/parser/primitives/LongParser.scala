package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.LongBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class LongParser extends Parser[LongBlock]{

  /**
    * Takes a line and checks to see ifs it is for this parser by using regex.
    */

  //var longTest:long = 10
  override def shouldParse(line: String): Boolean = {
    line.matches("var[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:long[ ]*([=][ ]*[0-9]+[ ]*)?")
  }

  /**
    * Take the superBlock and the tokenizer for the line and return a block of this parser's type.
    */
  override def parse(superBlock: Block, tokenizer: Tokenizer): LongBlock = {
    tokenizer.nextToken // skip "var"
    val name = tokenizer.nextToken.token // longTest
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "long"
    tokenizer.nextToken // skip "="
    var value = tokenizer.nextToken.token // 10

    if(value.equals(""))
      value = "0"

    new LongBlock(superBlock, name, value)
  }
}