package compiler.block.primitives

import compiler.block.Block

class CharacterBlock(val superBlock: Block, var name: String, var value: String) extends Block(superBlock, false, true) {
  private var `type`: String = "char"

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
    return "char " + name + " = '" + value + "';"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "char: " + name + " = " + value
  }
}