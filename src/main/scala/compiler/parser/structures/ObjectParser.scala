package compiler.parser.structures

import java.util.{ArrayList, List}

import compiler.block.Block
import compiler.block.structures.objects.ObjectBlock
import compiler.parser.Parser
import compiler.structure.parameter.Parameter
import compiler.tokenizer.Tokenizer

/**
  * Creation of a new instance of a class
  */
class ObjectParser extends Parser[ObjectBlock] {
  def shouldParse(line: String): Boolean = line.matches("var[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*:[a-zA-Z][a-zA-Z0-9]*[ ]*[=][ ]*new[ ]*[a-zA-Z][a-zA-Z0-9]*\\((.*)*\\)[ ]*")

  def parse(superBlock: Block, tokenizer: Tokenizer): ObjectBlock = {
    tokenizer.nextToken // skip "var"
    val variableName: String = tokenizer.nextToken.getToken
    tokenizer.nextToken // skip ":"
    val className: String = tokenizer.nextToken.getToken
    val operator: String = tokenizer.nextToken.getToken
    val newKeyword: String = tokenizer.nextToken.getToken
    val initClassName: String = tokenizer.nextToken.getToken

    tokenizer.nextToken // skip "("

    var nextToken: String = tokenizer.nextToken.getToken
    val paramType: String = ""
    var paramName: String = ""
    val parameters: List[Parameter] = new ArrayList[Parameter]
    while (nextToken != ")") {
      {

        if (nextToken == ",") {
          nextToken = tokenizer.nextToken.getToken
        } else {
          paramName = nextToken.trim
          parameters.add(new Parameter(paramType, paramName))
          nextToken = tokenizer.nextToken.getToken
        }
      }
    }
    return new ObjectBlock(superBlock, className, variableName, operator, newKeyword, initClassName, parameters.toArray(new Array[Parameter](parameters.size)))
  }
}
