package compiler.block.structures.objects

import compiler.Parameter
import compiler.block.Block

/**
  * Calling of a method within the class
  */
class MethodCallBlock(val superBlock: Block, var nameInit: String, var typeInit: String, var paramsInit: Array[Parameter]) extends Block(superBlock, false, false) {

  val name = nameInit
  val `type` = typeInit
  val params = paramsInit

  def getParameters: Array[Parameter] = {
    return params
  }

  def init() {
  }

  def getName: String = {
    return name
  }

  def getValue: String = {
    return null
  }

  def getType: String = {
    return `type`
  }

  def getOpeningCode: String = {
    return ""
  }

  def getBodyCode: String = {
    return name + "();"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return "method call: " + name + " ( " + paramString + ")"
  }
}