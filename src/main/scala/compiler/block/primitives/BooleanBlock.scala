package compiler.block.primitives

import compiler.block.Block

class BooleanBlock(var superBlockInit: Block, declaration: Boolean, name: String, value: String) extends Block(superBlockInit, false, true) {

  override def init() {}

  override def getName: String = name

  override def getValue: String = value

  override def getType: String = "boolean"

  override def getOpeningCode: String = asm.visitLdcInsn("new Boolean(" + value + ")") +
    asm.visitVarInsn("BASTORE", id)

  override def getClosingCode: String = ""

  override def toString: String = "boolean: " + name + " = " + value
}