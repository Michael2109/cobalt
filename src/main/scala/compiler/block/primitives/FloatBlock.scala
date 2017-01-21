package compiler.block.primitives

import compiler.block.Block

class FloatBlock(var superBlock: Block, var name: String, var value: String) extends Block(superBlock, false, true) {
  private var `type`: String = "float"

  def init() {
  }

  def getName: String = {
    return name
  }

  def setName(name: String) {
    this.name = name
  }

  def getValue: String = {
    return value
  }

  def setValue(value: String) {
    this.value = value
  }

  def getType: String = {
    return `type`
  }

  def setType(`type`: String) {
    this.`type` = `type`
  }

  def getOpeningCode: String = {
    return ""
  }

  def getBodyCode: String = {
    return ""
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "float: " + name + " = " + value
  }
}