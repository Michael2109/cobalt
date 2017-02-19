package compiler.block.primitives

import compiler.block.Block

class ArrayBlock( superBlockInit: Block, declaration : Boolean, name: String,  value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "array"


  def getOpeningCode: String = {
    return "array " + name + " = '" + value + "';"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "array: " + name + " = " + value

}