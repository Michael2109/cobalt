package test_classes.block.primitives

import test_classes.block.Block

class CharacterBlock( superBlockInit: Block, declaration : Boolean, name: String,  value: String) extends Block(superBlockInit, false, true) {

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