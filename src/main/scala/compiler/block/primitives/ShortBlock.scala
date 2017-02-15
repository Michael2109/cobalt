package compiler.block.primitives

import compiler.block.Block
import test_classes.block.Block


class ShortBlock(superBlockShort: Block, declaration : Boolean, name: String, value: String) extends Block(superBlockShort, false, true) {


  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "short"

  def getOpeningCode: String = {
    asm.visitLdcInsn("new Short((short)" + value + ")") +
      asm.visitVarInsn("SASTORE", id)
  }

  def getClosingCode: String = {
    ""
  }

  override def toString: String = "short: " + name + " = " + value

}
