package test_classes.block.primitives

import test_classes.block.Block

class IntegerBlock(superBlockInit: Block, declaration : Boolean, name: String, value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "int"

  def getOpeningCode: String = {
    asm.visitLdcInsn("new Integer(" + value + ")") +
      asm.visitVarInsn("ISTORE", id)
  }

  def getClosingCode: String = {
    ""
  }

  override def toString: String = "int: " + name + " = " + value

}