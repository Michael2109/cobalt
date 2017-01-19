package compiler.parser.structures.classes

import compiler.Parameter
import compiler.block.Block
import compiler.block.structures.classes.ClassBlock
import compiler.parser.Parser
import java.util.ArrayList
import java.util.List

import compiler.tokenizer.Tokenizer

class ClassParser extends Parser[ClassBlock] {
  def shouldParse(line: String): Boolean = {
    return line.matches("class[ ]+[a-zA-Z][a-zA-Z0-9]*\\((.*)*\\):")
  }

  def parse(superBlock: Block, tokenizer: Tokenizer): ClassBlock = {
    tokenizer.nextToken
    val className: String = tokenizer.nextToken.getToken
    tokenizer.nextToken // (
    var nextToken: String = tokenizer.nextToken.getToken
    var i: Int = 0
    var paramType: String = ""
    var paramName: String = ""
    val parameters: List[Parameter] = new ArrayList[Parameter]
    while (nextToken != ")") {
      {
        //	parameters += " " + nextToken + " ";
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
    return new ClassBlock(superBlock, className, parameters)
  }
}