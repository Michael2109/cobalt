package compiler.block.primitives

import compiler.block.Block

class LongGen(superBlockInit: Block, declaration : Boolean, name: String, value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "long"

  def getOpeningCode: String = {
    asm.visitLdcInsn("new Long(" + value + ")") +
      asm.visitVarInsn("LSTORE", id)
  }

  def getClosingCode: String = {
    ""
  }

  override def toString: String = "long: " + name + " = " + value

}