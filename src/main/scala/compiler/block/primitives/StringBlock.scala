package compiler.block.primitives

import compiler.block.Block

class StringBlock(superBlockInit: Block, declaration : Boolean, name: String, value: String) extends Block(superBlockInit, false, true) {

  override def init() {}

  override def getName: String = name

  override def getValue: String = value

  override def getType: String = "String"

  override def getOpeningCode: String = {
    asm.visitLdcInsn("\"" + value + "\"") +
      asm.visitVarInsn("ASTORE", id)
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "string: " + name + " = " + value

}