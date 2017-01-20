package compiler.block.structures.methods

import compiler.Parameter
import compiler.block.Block
import java.util.List

class ConstructorBlock(val superBlock: Block, var parameters: List[Parameter]) extends Block(superBlock, true, false) {
  def init() {
  }

  def getName: String = {
    return null
  }

  def getValue: String = {
    return null
  }

  def getType: String = {
    return null
  }

  def getOpeningCode: String = {
    return ""
  }

  def getBodyCode: String = {
    return ""
  }

  def getClosingCode: String = {
    return "mv.visitInsn(RETURN);                      // End the constructor method\n" + "mv.visitMaxs(0, 0);\n" + "mv.visitEnd();\n" + "}"
  }

  override def toString: String = {
    var paramString: String = ""
    import scala.collection.JavaConversions._
    for (parameter <- parameters) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return "constructor: ( " + paramString + ")"
  }
}