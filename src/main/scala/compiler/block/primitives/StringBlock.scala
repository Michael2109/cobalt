package compiler.block.primitives

import compiler.block.Block

class StringBlock(superBlockInit: Block, declaration : Boolean, name: String, value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "String"

  def getOpeningCode: String = {
    asm.visitLdcInsn("\"\"")
      asm.visitVarInsn("ASTORE", id)
  }

  def getClosingCode: String = {
    ""
  }

  override def toString: String = "string: " + name + " = " + value

}