package test_classes.block.primitives

import test_classes.block.Block

class DoubleBlock(superBlockInit: Block, declaration : Boolean, name: String,  value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "double"

  def getOpeningCode: String = {
    return "mv.visitLdcInsn(new Double(" + value + "));\n" +
      "mv.visitVarInsn(DSTORE, " + id + ");"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "double: " + name + " = " + value

}