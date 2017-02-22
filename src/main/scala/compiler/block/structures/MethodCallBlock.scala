package compiler.block.structures

import compiler.block.Block
import compiler.generators.structures.MethodCallGen
import compiler.structure.parameters.Parameter

/**
  * Calling of a method within the class
  */
class MethodCallBlock(var superBlockInit: Block, var name: String, var `type`: String, var params: Array[Parameter]) extends Block(superBlockInit, false, false) {

  def getParameters: Array[Parameter] = params

  def init() {}

  def getName: String = name

  def getValue: String = ""

  def getType: String = `type`

  def getOpeningCode: String = name + "();"

  def getClosingCode: String = ""

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return "method call: " + name + " ( " + paramString + ")"
  }
}