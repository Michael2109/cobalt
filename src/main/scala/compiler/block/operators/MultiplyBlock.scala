package compiler.block.operators

import compiler.block.Block

class MultiplyBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, false) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "multiply"

  def getOpeningCode: String = {
    return "mv.visitLdcInsn(" + value + ");\n" + "mv.visitVarInsn(ILOAD," + id + ");\n" + "mv.visitInsn(IMUL);\n" + "mv.visitVarInsn(ISTORE," + id + ");\n"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "multiply: " + name
  }
}