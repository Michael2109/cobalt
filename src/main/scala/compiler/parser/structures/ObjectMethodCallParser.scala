package compiler.parser.structures

import compiler.Parameter
import compiler.Utils
import compiler.block.Block
import compiler.block.structures.objects.ObjectMethodCallBlock
import compiler.parser.Parser
import java.util.ArrayList
import java.util.List

import compiler.tokenizer.Tokenizer

/**
  * Parses calling of an objects method
  */
class ObjectMethodCallParser extends Parser[ObjectMethodCallBlock] {
  def shouldParse(line: String): Boolean = line.matches("[a-zA-Z][a-zA-Z0-9]*\\.[a-zA-Z][a-zA-Z0-9]*\\((.*)*\\)[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): ObjectMethodCallBlock = {
    val variableName: String = tokenizer.nextToken.getToken // Get the string value of the next token.
    tokenizer.nextToken
    val methodName: String = tokenizer.nextToken.getToken
    tokenizer.nextToken // ")"
    var nextToken: String = tokenizer.nextToken.getToken
    val paramType: String = ""
    var paramName: String = ""
    val parameters: List[Parameter] = new ArrayList[Parameter]
    while (nextToken != ")") {
      {
        if (nextToken == ",") {
          nextToken = tokenizer.nextToken.getToken
        } else {
          // todo find the paramType. Utilities add a method to get the type
          paramName = nextToken.trim
          parameters.add(new Parameter(paramType, paramName))
          nextToken = tokenizer.nextToken.getToken
        }
      }
    }
    return new ObjectMethodCallBlock(superBlock, variableName, methodName, parameters.toArray(new Array[Parameter](parameters.size)))
  }
}
