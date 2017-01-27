package compiler.block.primitives

import compiler.block.Block

class CharacterBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "char"


  def getOpeningCode: String = {
    return "char " + name + " = '" + value + "';"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "char: " + name + " = " + value

}