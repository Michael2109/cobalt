package compiler.block.primitives

import compiler.block.Block

class LongBlock(superBlockInit: Block, declaration : Boolean, name: String, value: String) extends Block(superBlockInit, false, true) {

  override def init() {}

  override def getName: String = name

  override def getValue: String = value

  override def getType: String = "long"

  override def getOpeningCode: String = {
    asm.visitLdcInsn("new Long(" + value + ")") +
      asm.visitVarInsn("LSTORE", id)
  }

  override  def getClosingCode: String = {
    ""
  }

  override def toString: String = "long: " + name + " = " + value

}