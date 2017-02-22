package compiler.block.primitives

import compiler.block.Block

class CharacterBlock( superBlockInit: Block, declaration : Boolean, name: String,  value: String) extends Block(superBlockInit, false, true) {

  override def init() {}

  override def getName: String = name

  override def getValue: String = value

  override def getType: String = "char"


  override def getOpeningCode: String = {
    return "char " + name + " = '" + value + "';"
  }

  override  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "char: " + name + " = " + value

}