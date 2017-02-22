package compiler.block.primitives

import compiler.block.Block

class ArrayBlock( superBlockInit: Block, declaration : Boolean, name: String,  value: String) extends Block(superBlockInit, false, true) {

  override def init() {}

  override def getName: String = name

  override def getValue: String = value

  override def getType: String = "array"


  override  def getOpeningCode: String = {
    return "array " + name + " = '" + value + "';"
  }

  override def getClosingCode: String = {
    return ""
  }

  override def toString: String = "array: " + name + " = " + value

}