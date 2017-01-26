package compiler.block.primitives

import asm.ASMGenerator
import compiler.block.Block

class IntegerBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, true) {

  def init() {}

  def getName: String = name

  def getValue: String = value

  def getType: String = "int"

  def getOpeningCode: String = {
    return ASMGenerator.getInstance.visitLdcInsn(value) +
      ASMGenerator.getInstance.visitVarInsn(id)
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "int: " + name + " = " + value

}