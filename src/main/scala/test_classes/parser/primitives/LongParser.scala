package test_classes.parser.primitives

import test_classes.block.Block
import test_classes.block.primitives.LongBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

class LongParser extends Parser[LongBlock]{

  /**
    * Takes a line and checks to see ifs it is for this parser by using regex.
    */

  //var longTest:long = 10
  override def shouldParse(line: String): Boolean = line.matches("(val|var)[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*long[ ]*([=][ ]*[0-9]+[ ]*)?")

  /**
    * Take the superBlock and the tokenizer for the line and return a block of this parser's type.
    */
  override def parse(superBlock: Block, tokenizer: Tokenizer): LongBlock = {
    val declaration: Boolean = tokenizer.nextToken.token == "val" // "val" or "var"
    val name = tokenizer.nextToken.token // longTest
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "long"
    tokenizer.nextToken // skip "="
    var value = tokenizer.nextToken.token // 10

    if(value.equals(""))
      value = "0"

    new LongBlock(superBlock, declaration,name, value)
  }
}