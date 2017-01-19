package compiler.parser.ifs

import compiler.block.Block
import compiler.block.ifs.IfBlock
import compiler.block.loops.WhileBlock
import compiler.parser.Parser
import compiler.tokenizer.Token
import compiler.tokenizer.TokenData
import compiler.tokenizer.TokenType
import compiler.tokenizer.Tokenizer

class IfParser extends Parser[IfBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("if \\((.*)*\\):")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): IfBlock = {
    tokenizer.nextToken //skip if
    tokenizer.nextToken // skip (

    // Loop through getting each parameter and  adding to a String
    var statement: String = ""
    var nextToken: String = tokenizer.nextToken.getToken
    while (nextToken != ")") {
      {
        if (nextToken == "=") statement += nextToken
        else statement += " " + nextToken + " "
        nextToken = tokenizer.nextToken.getToken
      }
    }
    return new IfBlock(superBlock, statement.trim.replaceAll(" +", " "))
  }
}