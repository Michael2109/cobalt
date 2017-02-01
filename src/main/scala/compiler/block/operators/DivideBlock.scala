package compiler.block.operators

import compiler.block.Block

class DivideBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, false) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "divide"

  def getOpeningCode: String = {
    return "mv.visitLdcInsn(" + value + ");\n" + "mv.visitVarInsn(ILOAD," + id + ");\n" + "mv.visitInsn(IDIV);\n" + "mv.visitVarInsn(ISTORE," + id + ");\n"
  }

  def getClosingCode: String = {
    return null
  }

  override def toString: String = {
    return "divide: " + name
  }

}