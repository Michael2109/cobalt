package compiler.parser.structures.classes

import compiler.block.Block
import compiler.block.structures.classes.ClassBlock
import compiler.parser.Parser
import java.util.ArrayList
import java.util.List

import compiler.structure.parameter.{Parameter, Parameters}
import compiler.tokenizer.Tokenizer

class ClassParser extends Parser[ClassBlock] {
  def shouldParse(line: String): Boolean = line.matches("class[ ]+[a-zA-Z][a-zA-Z0-9]*\\((.*)*\\):")

  def parse(superBlock: Block, tokenizer: Tokenizer): ClassBlock = {
    tokenizer.nextToken
    val className: String = tokenizer.nextToken.getToken
    tokenizer.nextToken // (
    var nextToken: String = tokenizer.nextToken.getToken

    var paramString = ""
    while (nextToken != ")") {
      paramString += nextToken
      nextToken = tokenizer.nextToken.getToken
    }

    val parameters = new Parameters().getParameters(paramString)

    return new ClassBlock(superBlock, className, parameters.toArray)
  }
}
