package compiler.parser.structures.methods

import compiler.Parameter
import compiler.block.Block
import compiler.block.structures.methods.MethodBlock
import compiler.parser.Parser
import java.util.ArrayList
import java.util.List

import compiler.tokenizer.Tokenizer

class MethodParser extends Parser[MethodBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("void[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*\\((.*)*\\)[ ]*:")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): MethodBlock = {
    val `type`: String = tokenizer.nextToken.getToken // method return type
    val name: String = tokenizer.nextToken.getToken // method name
    tokenizer.nextToken // "("
    var nextToken: String = tokenizer.nextToken.getToken
    var i: Int = 0
    var paramType: String = ""
    var paramName: String = ""
    val parameters: List[Parameter] = new ArrayList[Parameter]
    while (nextToken != ")") {
      {
        if (nextToken == ",") {
          nextToken = tokenizer.nextToken.getToken
        } else {
          if (i % 2 == 0) {
            paramType = nextToken.trim
          }
          else {
            paramName = nextToken.trim
            parameters.add(new Parameter(paramType, paramName))
          }
          nextToken = tokenizer.nextToken.getToken
          i += 1
        }
      }
    }
    return new MethodBlock(superBlock, name, `type`, parameters.toArray(new Array[Parameter](parameters.size)))
  }
}