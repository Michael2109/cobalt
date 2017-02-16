package compiler.parser.structures.kinds

import compiler.block.Block
import compiler.block.structures.kinds.ObjectBlock
import compiler.parser.Parser
import compiler.structure.parameters.Parameters
import compiler.tokenizer.Tokenizer

class ObjectParser extends Parser[ObjectBlock] {

  def shouldParse(line: String): Boolean = {
    println(line + ":" + line.matches("object[ ]+[a-zA-Z][a-zA-Z0-9]*:[ ]*"))
    line.matches("object[ ]+[a-zA-Z][a-zA-Z0-9]*:[ ]*")
  }
  def parse(superBlock: Block, tokenizer: Tokenizer): ObjectBlock = {
    tokenizer.nextToken
    val objectName: String = tokenizer.nextToken.token

    val parameters = new Parameters().getParameters("")

    val parentClass = "java/lang/Object"


    val implementedClasses = ""



    return new ObjectBlock(superBlock, objectName, parameters.toArray, parentClass, implementedClasses)
  }
}
