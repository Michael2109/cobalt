package compiler.block.primitives

import asm.ASMGenerator
import compiler.block.Block

class IntegerBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, true) {
  private var `type`: String = "int"

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
    return ASMGenerator.getInstance.visitLdcInsn(value) +
      ASMGenerator.getInstance.visitVarInsn(id)
  }


  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "int: " + name + " = " + value
  }
}