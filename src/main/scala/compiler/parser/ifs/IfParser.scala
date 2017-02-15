package compiler.parser.ifs

import compiler.block.Block
import compiler.block.ifs.IfBlock
import compiler.block.loops.WhileBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer
import test_classes.block.Block
import test_classes.block.ifs.IfBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

class IfParser extends Parser[IfBlock] {

  def shouldParse(line: String): Boolean = line.matches("if \\((.*)*\\):")

  def parse(superBlock: Block, tokenizer: Tokenizer): IfBlock = {
    tokenizer.nextToken //skip if
    tokenizer.nextToken // skip (

    // Loop through getting each parameter and  adding to a String
    var statement: String = ""
    var nextToken: String = tokenizer.nextToken.token
    while (nextToken != ")") {
      {
        if (nextToken == "=") statement += nextToken
        else statement += " " + nextToken + " "
        nextToken = tokenizer.nextToken.token
      }
    }
    return new IfBlock(superBlock, statement.trim.replaceAll(" +", " "))
  }
}
