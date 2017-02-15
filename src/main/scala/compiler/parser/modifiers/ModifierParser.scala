package compiler.parser.modifiers

import compiler.block.Block
import compiler.block.modifiers.ModifierBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class ModifierParser extends Parser[ModifierBlock] {

  def shouldParse(line: String): Boolean = {
    line.matches("(public|private|protected):")
  }
  def parse(superBlock: Block, tokenizer: Tokenizer): ModifierBlock = new ModifierBlock(superBlock, tokenizer.nextToken.token)

}
