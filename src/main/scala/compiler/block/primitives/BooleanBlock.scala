package compiler.block.primitives

import compiler.block.Block

class BooleanBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "boolean"

  def getOpeningCode: String = ""

  def getClosingCode: String = ""

  override def toString: String = "boolean: " + name + " = " + value
}