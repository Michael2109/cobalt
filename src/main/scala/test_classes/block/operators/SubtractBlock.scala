package test_classes.block.operators

import test_classes.block.Block

class SubtractBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, false) {
  private var `type`: String = "subtract"

  def init() {
  }

  def getName: String = name


  def getValue: String = value


  def getType: String = "subtract"


  def getOpeningCode: String = {
    return "mv.visitLdcInsn(" + value + ");\n" + "mv.visitVarInsn(ILOAD," + id + ");\n" + "mv.visitInsn(ISUB);\n" + "mv.visitVarInsn(ISTORE," + id + ");\n"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "subtract: " + name
  }
}