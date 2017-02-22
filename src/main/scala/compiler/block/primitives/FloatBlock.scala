package compiler.block.primitives

import compiler.block.Block

class FloatBlock(superBlockInit: Block, declaration : Boolean, name: String, value: String) extends Block(superBlockInit, false, true) {

  override def init() {}

  override def getName: String = {
    name
  }

  override def getValue: String = {
    value
  }

  override def getType: String = {
    "float"
  }

  override def getOpeningCode: String = {
    asm.visitLdcInsn("new Float(" + value + ")") +
      asm.visitVarInsn("FSTORE", id);
  }

  override def getClosingCode: String = {
    ""
  }

  override def toString: String = "float: " + name + " = " + value

}