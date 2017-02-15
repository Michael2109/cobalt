package compiler.block.structures

import compiler.block.Block
import compiler.structure.parameters.Parameter
import test_classes.block.Block
import test_classes.structure.parameters.Parameter

/**
  * Calling of a method within the class
  */
class MethodCallBlock(var superBlockInit: Block, var nameInit: String, var typeInit: String, var paramsInit: Array[Parameter]) extends Block(superBlockInit, false, false) {

  private val _name = nameInit
  private val _type = typeInit
  private val _params = paramsInit

  def getParameters: Array[Parameter] = _params

  def init() {}

  def getName: String = _name

  def getValue: String = ""

  def getType: String = _type

  def getOpeningCode: String = _name + "();"

  def getClosingCode: String = ""

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- _params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return "method call: " + _name + " ( " + paramString + ")"
  }
}