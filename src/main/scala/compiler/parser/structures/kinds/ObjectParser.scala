package compiler.parser.structures.kinds

import compiler.block.Block
import compiler.block.structures.kinds.ObjectBlock
import compiler.parser.Parser
import compiler.structure.parameters.Parameters
import compiler.tokenizer.Tokenizer
import test_classes.block.Block
import test_classes.block.structures.kinds.{ClassBlock, ObjectBlock}
import test_classes.parser.Parser
import test_classes.structure.parameters.Parameters
import test_classes.tokenizer.Tokenizer

class ObjectParser extends Parser[ObjectBlock] {
  def shouldParse(line: String): Boolean = line.matches("object[ ]+[a-zA-Z][a-zA-Z0-9]*\\((.*)*\\)([ ]+extends[ ]+[a-zA-Z][a-zA-Z0-9]*)?:")

  def parse(superBlock: Block, tokenizer: Tokenizer): ObjectBlock = {
    tokenizer.nextToken
    val className: String = tokenizer.nextToken.token
    tokenizer.nextToken // (
    var nextToken: String = tokenizer.nextToken.token

    var paramString = ""
    while (nextToken != ")") {
      paramString += nextToken
      nextToken = tokenizer.nextToken.token
    }

    val parameters = new Parameters().getParameters(paramString)

    var parentClass = ""

    if (tokenizer.nextToken.token == "extends") {
      parentClass = tokenizer.nextToken.token
    } else {
      parentClass = "java/lang/Object"
    }

    var implementedClasses = ""
    if (tokenizer.nextToken.token == "implements") {
      implementedClasses = tokenizer.nextToken.token
    }


    return new ObjectBlock(superBlock, className, parameters.toArray, parentClass, implementedClasses)
  }
}
