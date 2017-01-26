package compiler.block.operators

import compiler.block.Block

class DivideBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, false) {
  private var `type`: String = "divide"

  def init() {
  }

  def getName: String = {
    return name
  }

  def setName(name: String) {
    this.name = name
  }

  def getValue: String = {
    return value
  }

  def setValue(value: String) {
    this.value = value
  }

  def getType: String = {
    return `type`
  }

  def setType(`type`: String) {
    this.`type` = `type`
  }

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