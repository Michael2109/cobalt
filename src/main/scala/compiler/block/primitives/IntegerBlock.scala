package compiler.block.primitives

import compiler.block.Block

class IntegerBlock(superBlockInit: Block, declaration : Boolean, name: String, value: String) extends Block(superBlockInit, false, true) {

  override def init() {}

  override def getName: String = name

  override def getValue: String = value

  override  def getType: String = "int"

  override def getOpeningCode: String = {
    asm.visitLdcInsn("new Integer(" + value + ")") +
      asm.visitVarInsn("ISTORE", id)
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "int: " + name + " = " + value

}