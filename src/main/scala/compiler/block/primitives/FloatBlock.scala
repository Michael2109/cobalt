package compiler.block.primitives

import compiler.block.Block

class FloatBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = {
    return name
  }

  def getValue: String = {
    return value
  }

  def getType: String = {
    return "float"
  }

  def getOpeningCode: String = {
    return asm.visitLdcInsn("new Float(" + value + ")") +
      asm.visitVarInsn("FSTORE", id);
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "float: " + name + " = " + value

}