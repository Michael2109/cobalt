package compiler.parser

import compiler.block.Block
import compiler.tokenizer.Tokenizer

abstract class Parser[T <: Block] {
  /**
    * Takes a line and checks to see ifs it is for this parser by using regex.
    */
  def shouldParse(line: String): Boolean

  /**
    * Take the superBlock and the tokenizer for the line and return a block of this parser's type.
    */
  def parse(superBlock: Block, tokenizer: Tokenizer): Block
}