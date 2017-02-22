package compiler.block.primitives

import compiler.block.Block

class ShortBlock(superBlockShort: Block, declaration : Boolean, name: String, value: String) extends Block(superBlockShort, false, true) {


  override  def init() {}

  override def getName: String = name

  override  def getValue: String = value

  override def getType: String = "short"

  override def getOpeningCode: String = {
    asm.visitLdcInsn("new Short((short)" + value + ")") +
      asm.visitVarInsn("SASTORE", id)
  }

  override  def getClosingCode: String = {
    ""
  }

  override def toString: String = "short: " + name + " = " + value

}
