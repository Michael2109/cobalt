package test_classes.parser.primitives

import test_classes.block.Block
import test_classes.block.primitives.ShortBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer


class ShortParser extends Parser[ShortBlock]{

  //TODO show how to set default values

  /**
    * Takes a line and checks to see ifs it is for this parser by using regex.
    */
  override def shouldParse(line: String): Boolean = line.matches("(val|var)[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[ ]*short[ ]*[=][ ]*[0-9]+[ ]*")

  /**
    * Take the superBlock and the tokenizer for the line and return a block of this parser's type.
    */
  override def parse(superBlock: Block, tokenizer: Tokenizer): ShortBlock = {

    val declaration: Boolean = tokenizer.nextToken.token == "val" // "val" or "var"
    val name: String = tokenizer.nextToken.token
    tokenizer.nextToken // skip ":"
    tokenizer.nextToken // skip "short"
    tokenizer.nextToken // skip "="
    val value: String = tokenizer.nextToken.token
    new ShortBlock(superBlock, declaration,name, value)

  }

}
