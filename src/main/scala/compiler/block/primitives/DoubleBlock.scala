package compiler.block.primitives

import compiler.block.Block

class DoubleBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "double"

  def getOpeningCode: String = {
    return ""
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "double: " + name + " = " + value

}