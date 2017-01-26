package compiler.block.ifs

import compiler.Parameter
import compiler.Utils
import compiler.block.Block
import compiler.symbol_table.SymbolTable

class IfBlock(var superBlockInit: Block, var name: String) extends Block(superBlockInit, true, false) {
  val split: Array[String] = name.split(" ")
  //  x == 10
  if (split.length > 1) {
    pointer = split(0)
    pointer = "" + SymbolTable.getInstance.getValue(Utils.getMethod(this), split(0)).getId
    operator = split(1)
    value = split(2)
    if (operator == "==") {
      byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == "<") {
      byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == ">") {
      byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == "<=") {
      byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == ">=") {
      byteCodeOp = "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else {
      System.out.println("Error: Disallowed Operator" + this.getClass)
    }
  }
  else {
    //boolean value
    value = name
  }
  private val `type`: String = "if"
  private val params: Array[Parameter] = null
  private[ifs] var pointer: String = null
  private[ifs] var operator: String = null
  private[ifs] var value: String = null
  private[ifs] var byteCodeOp: String = null

  def getParameters: Array[Parameter] = {
    return params
  }

  def getType: String = {
    return `type`
  }

  def getClosingCode: String = {
    return "mv.visitLabel(l" + id + ");"
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
    return "mv.visitVarInsn(ILOAD," + pointer + ");" + "mv.visitLdcInsn(" + value + ");\n" + "Label l" + id + " = new Label();\n" + byteCodeOp
  }

  override def toString: String = {
    return "if " + name
  }
}