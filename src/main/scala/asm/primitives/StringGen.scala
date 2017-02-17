package compiler.block.primitives

import compiler.block.Block

class StringGen(superBlockInit: Block, declaration : Boolean, name: String, value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "String"

  def getOpeningCode: String = {
    return asm.visitLdcInsn("new String(\"" + value + "\")") +
      asm.visitVarInsn("ASTORE", id)
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "int: " + name + " = " + value

}