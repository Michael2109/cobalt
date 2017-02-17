package compiler.block.ifs

import compiler.Utils
import compiler.block.Block
import compiler.structure.parameters.Parameter
import compiler.symbol_table.SymbolTable

/**
  * Represents an if statement
  *
  * @param superBlockInit
  * @param nameInit
  */
class IfGen(var superBlockInit: Block, var nameInit: String) extends Block(superBlockInit, true, false) {

  private val params: Array[Parameter] = null
  private val _name = nameInit
  private val split: Array[String] = _name.split(" ")
  private var pointer: String = null
  private var operator: String = null
  private var value: String = null
  private var byteCodeOp: String = null

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
    value = _name
  }

  def getParameters: Array[Parameter] = params

  def getName: String = _name

  def getType: String = "if"

  def getValue: String = null

  def init() {
  }

  def getOpeningCode: String = {
    return asm.visitVarInsn("ILOAD", pointer) +
      asm.visitLdcInsn(value) +
      asm.newLabel("l" + id) +
      byteCodeOp
  }

  def getClosingCode: String = {
    return asm.visitLabel("l" + id)
  }

  override def toString: String = "if " + _name

}