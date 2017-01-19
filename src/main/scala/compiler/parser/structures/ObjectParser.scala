package compiler.parser.structures

import compiler.block.Block
import compiler.block.structures.objects.ObjectBlock
import compiler.parser.Parser
import compiler.tokenizer.Tokenizer

/**
  * Creation of a new instance of a class
  */
class ObjectParser extends Parser[ObjectBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("[a-zA-Z][a-zA-Z0-9]* [a-zA-Z][a-zA-Z0-9]* [=][ ]*new[ ]*[a-zA-Z][a-zA-Z0-9]*\\(\\)")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): ObjectBlock = {
    val className: String = tokenizer.nextToken.getToken
    val variableName: String = tokenizer.nextToken.getToken
    val operator: String = tokenizer.nextToken.getToken
    val newKeyword: String = tokenizer.nextToken.getToken
    val initClassName: String = tokenizer.nextToken.getToken
    return new ObjectBlock(superBlock, className, variableName, operator, newKeyword, initClassName)
  }
}