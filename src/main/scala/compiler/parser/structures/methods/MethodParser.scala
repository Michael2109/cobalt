package compiler.parser.structures.methods

import compiler.block.Block
import compiler.block.structures.methods.MethodBlock
import compiler.parser.Parser
import java.util.ArrayList
import java.util.List

import compiler.structure.parameter.{Parameter, Parameters}
import compiler.tokenizer.Tokenizer

class MethodParser extends Parser[MethodBlock] {

  def shouldParse(line: String): Boolean = line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*\\((.*)*\\)[ ]*<-[ ]*[a-zA-Z][a-zA-Z0-9]*[ ]*:")

  def parse(superBlock: Block, tokenizer: Tokenizer): MethodBlock = {

    val name: String = tokenizer.nextToken.getToken // method name
    tokenizer.nextToken // "("

    var nextToken = tokenizer.nextToken.getToken
    var paramString = ""
    while (nextToken != ")") {
      paramString += nextToken
      nextToken = tokenizer.nextToken.getToken
    }
    val parameters = new Parameters().getParameters(paramString)

    println(tokenizer.nextToken) // skip ")"

    println(tokenizer.nextToken) // skip "<-"

    val `type`: String = tokenizer.nextToken.getToken // method return type
    return new MethodBlock(superBlock, name, `type`, parameters.toArray)
  }
}
