package compiler.block.primitives

import compiler.block.Block

class IntegerBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "int"

  def getOpeningCode: String = {
    asm.visitLdcInsn("new Integer(" + value + ")") +
      asm.visitVarInsn("ISTORE", id)
  }

  def getClosingCode: String = {
    ""
  }

  override def toString: String = "int: " + name + " = " + value

}