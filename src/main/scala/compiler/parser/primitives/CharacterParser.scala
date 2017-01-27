package compiler.parser.primitives

import compiler.block.Block
import compiler.block.primitives.CharacterBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

class CharacterParser extends Parser[CharacterBlock] {

  def shouldParse(line: String): Boolean = line.matches("char [a-zA-Z][a-zA-Z0-9]* [=] '[a-zA-Z0-9]'")

  def parse(superBlock: Block, tokenizer: Tokenizer): CharacterBlock = {
    tokenizer.nextToken // skip "char"
    val name: String = tokenizer.nextToken.getToken //get name
    tokenizer.nextToken // skip "="
    //System.out.println(tokenizer.nextToken().getToken());  // skip '
    tokenizer.nextToken
    val value: String = tokenizer.nextToken.getToken
    return new CharacterBlock(superBlock, name, value)
  }
}
