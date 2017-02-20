package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.ArrayBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class ArrayParser extends Parser[ArrayBlock]{
  /**
    * Takes a line and checks to see ifs it is for this parser by using regex.
    */
  override def shouldParse(line: String): Boolean = false

  /**
    * Take the superBlock and the tokenizer for the line and return a block of this parser's type.
    */
  override def parse(superBlock: Block, tokenizer: Tokenizer): Block = null
}
