package compiler.parser.loops

import compiler.block.Block
import compiler.block.loops.ForBlock
import compiler.parser.Parser
import compiler.tokenizer.{Token, Tokenizer}

/*
need to make parameter single variable names instead as cant define a variable in an ifs...
 */
class ForParser extends Parser[ForBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("for \\([a-zA-Z][a-zA-Z0-9]* [a-zA-Z][a-zA-Z0-9]* = [0-9]+([.][0-9]*)?; [a-zA-Z][a-zA-Z0-9]* [<|>|<=|>=|==] [0-9]+([.][0-9]*)?; [a-zA-Z][a-zA-Z0-9]*\\+\\+\\):")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): ForBlock = {
    tokenizer.nextToken //skip ifs
    tokenizer.nextToken // skip (
    val first: Token = tokenizer.nextToken
    return new ForBlock(superBlock, first.getToken)
  }
}