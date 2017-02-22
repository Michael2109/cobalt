package compiler.block.primitives

import compiler.block.Block

class DoubleBlock(superBlockInit: Block, declaration : Boolean, name: String,  value: String) extends Block(superBlockInit, false, true) {

  override def init() {}

  override def getName: String = name

  override  def getValue: String = value

  override def getType: String = "double"

  override def getOpeningCode: String = {
    "mv.visitLdcInsn(new Double(" + value + "));\n" +
      "mv.visitVarInsn(DSTORE, " + id + ");"
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "double: " + name + " = " + value

}