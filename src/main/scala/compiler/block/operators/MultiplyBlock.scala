package compiler.block.operators

import compiler.block.Block

class MultiplyBlock(var superBlock: Block, var name: String, var value: String) extends Block(superBlock, false, false) {
  private var `type`: String = "multiply"

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
    return null
  }

  def getBodyCode: String = {
    return "mv.visitLdcInsn(" + value + ");\n" + "mv.visitVarInsn(ILOAD," + getId + ");\n" + "mv.visitInsn(IMUL);\n" + "mv.visitVarInsn(ISTORE," + getId + ");\n"
  }

  def getClosingCode: String = {
    return null
  }

  override def toString: String = {
    return "multiply: " + name
  }
}