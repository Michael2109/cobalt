package compiler.block.loops

import compiler.Utils
import compiler.block.Block
import compiler.symbol_table.SymbolTable

class WhileBlock(val superBlock: Block, var name: String) extends Block(superBlock, true, false) {
  val split: Array[String] = name.split(" ")
  //  x == 10
  if (split.length > 1) {
    pointer = split(0)
    pointer = "" + SymbolTable.getInstance.getValue(Utils.getMethod(this), split(0)).getId
    operator = split(1)
    value = split(2)
    if (operator == "==") {
      byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + getId + ");\n"
    }
    else if (operator == "<") {
      byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + getId + ");\n"
    }
    else if (operator == ">") {
      byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + getId + ");\n"
    }
    else if (operator == "<=") {
      byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + getId + ");\n"
    }
    else if (operator == ">=") {
      byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + getId + ");\n"
    }
    else {
      System.out.println("Error: Disallowed Operator" + this.getClass)
    }
  }
  else {
    //boolean value
    value = name
  }
  private val `type`: String = "while"
  private[loops] var pointer: String = null
  private[loops] var operator: String = null
  private[loops] var value: String = null
  private[loops] var byteCodeOp: String = null

  def getType: String = {
    return `type`
  }

  def getClosingCode: String = {
    return "mv.visitJumpInsn(GOTO, start" + getId + ");\n" + "mv.visitLabel(l" + getId + ");\n"
  }

  def getValue: String = {
    return null
  }

  def init() {
  }

  def getName: String = {
    return name
  }

  def getOpeningCode: String = {
    return "Label start" + getId + " = new Label();\n" + "mv.visitLabel(start" + getId + ");\n" + "mv.visitVarInsn(ILOAD," + pointer + ");\n" + "mv.visitLdcInsn(" + value + ");\n" + "Label l" + getId + " = new Label();\n" + byteCodeOp
  }

  def getBodyCode: String = {
    return ""
  }

  override def toString: String = {
    return "while: " + name
  }
}