package compiler.parser.structures.methods

import compiler.block.Block
import compiler.block.structures.methods.MethodBlock
import compiler.parser.Parser
import java.util.ArrayList
import java.util.List

import compiler.structure.parameters.{Parameter, Parameters}
import compiler.tokenizer.Tokenizer

class MethodParser extends Parser[MethodBlock] {

  def shouldParse(line: String): Boolean = line.matches("def[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*\\((.*)*\\)[ ]*<-[ ]*[a-zA-Z][a-zA-Z0-9]*[ ]*:")

  def parse(superBlock: Block, tokenizer: Tokenizer): MethodBlock = {

    tokenizer.nextToken // skip "def"
    val name: String = tokenizer.nextToken.token // method name
    tokenizer.nextToken // "("
    var nextToken = tokenizer.nextToken.token
    var paramString = ""
    while (!nextToken.equals(")")) {
      paramString += nextToken
      nextToken = tokenizer.nextToken.token
    }

    val parameters = new Parameters().getParameters(paramString)

    tokenizer.nextToken // skip "<"

    tokenizer.nextToken // skip "-"

    val `type`: String = tokenizer.nextToken.token // method return type
    return new MethodBlock(superBlock, name, `type`, parameters.toArray)
  }
}
