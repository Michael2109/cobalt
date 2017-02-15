package test_classes.parser.structures.kinds

import test_classes.block.Block
import test_classes.block.structures.kinds.ClassBlock
import test_classes.parser.Parser
import test_classes.structure.parameters.Parameters
import test_classes.tokenizer.Tokenizer

class ClassParser extends Parser[ClassBlock] {
  def shouldParse(line: String): Boolean = line.matches("class[ ]+[a-zA-Z][a-zA-Z0-9]*\\((.*)*\\)([ ]+extends[ ]+[a-zA-Z][a-zA-Z0-9]*)?:")

  def parse(superBlock: Block, tokenizer: Tokenizer): ClassBlock = {
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


    return new ClassBlock(superBlock, className, parameters.toArray, parentClass, implementedClasses)
  }
}
