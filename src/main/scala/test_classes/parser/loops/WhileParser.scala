package test_classes.parser.loops

import test_classes.block.Block
import test_classes.block.loops.WhileBlock
import test_classes.parser.Parser
import test_classes.tokenizer.Tokenizer

/*
need to make parameter single variable names instead as cant define a variable in an ifs...
 */
class WhileParser extends Parser[WhileBlock] {

  def shouldParse(line: String): Boolean = {
    return line.matches("while[ ]+\\((.*)*\\)[:]?")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): WhileBlock = {

    tokenizer.nextToken //skip "while"
    tokenizer.nextToken // skip "("

    var statement: String = ""
    var nextToken: String = tokenizer.nextToken.token
    while (!nextToken.equals(")") && nextToken != "") {
      {
        if (nextToken == "=") statement += nextToken
        else statement += " " + nextToken + " "
        nextToken = tokenizer.nextToken.token
      }
    }
    return new WhileBlock(superBlock, statement.trim.replaceAll(" +", " "))
  }
}
