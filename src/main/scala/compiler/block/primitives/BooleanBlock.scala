package compiler.block.primitives

import compiler.block.Block
import test_classes.block.Block

class BooleanBlock(var superBlockInit: Block, declaration: Boolean, name: String, value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "boolean"

  def getOpeningCode: String = asm.visitLdcInsn("new Boolean(" + value + ")") +
    asm.visitVarInsn("BASTORE", id)

  def getClosingCode: String = ""

  override def toString: String = "boolean: " + name + " = " + value
}