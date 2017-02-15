package test_classes.parser.modifiers

import test_classes.block.Block
import test_classes.block.modifiers.ModifierBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

class ModifierParser extends Parser[ModifierBlock] {

  def shouldParse(line: String): Boolean = {
    line.matches("(public|private|protected):")
  }
  def parse(superBlock: Block, tokenizer: Tokenizer): ModifierBlock = new ModifierBlock(superBlock, tokenizer.nextToken.token)

}
