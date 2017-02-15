package test_classes.parser.prints

import test_classes.block.Block
import test_classes.block.prints.PrintBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

class PrintParser extends Parser[PrintBlock] {
  var printVariable: Boolean = false

  def shouldParse(line: String): Boolean = {
    // Decide whether printing a variable or a string
    if (line.matches("print[ ]*\\([\"].*[\"]\\)")) {
      printVariable = false
      return true
    }
    else if (line.matches("print[ ]*\\(.*\\)")) {
      printVariable = true
      return true
    }
    return false
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): PrintBlock = {
    tokenizer.nextToken // skip print
    tokenizer.nextToken // skip (
    val value: String = tokenizer.nextToken.token
    return new PrintBlock(superBlock, value, printVariable)
  }
}
