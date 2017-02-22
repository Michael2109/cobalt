package compiler.block.ifs

import compiler.block.Block
import compiler.generators.ifs.IfGen
import compiler.structure.parameters.Parameter
import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils

/**
  * Represents an if statement
  *
  * @param superBlockInit
  * @param nameInit
  */
class IfBlock(var superBlockInit: Block, var nameInit: String) extends Block(superBlockInit, true, false) {

  private val params: Array[Parameter] = null
  private val _name = nameInit
  private val split: Array[String] = _name.split(" ")
  private val pointer: String = "" + SymbolTable.getInstance.getValue(Utils.getMethod(this), split(0)).getId
  private val operator: String = if(split.length > 1) split(1) else ""
  private val value: String = if(split.length > 1) split(2) else _name

  private val byteCodeOp: String = {
    if (operator == "==") {
      "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == "<") {
      "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == ">") {
      "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == "<=") {
      "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else if (operator == ">=") {
      "mv.visitJumpInsn(IF_ICMPGE, l" + id + ");\n"
    }
    else {
      throw new RuntimeException("Error: Disallowed Operator")
    }
  }

  def getParameters: Array[Parameter] = params

  def getName: String = _name

  def getType: String = "if"

  def getValue: String = null

  def init() {
  }

  def getOpeningCode: String = {
    return IfGen.getOpeningCode(pointer, value, id, byteCodeOp)
  }

  def getClosingCode: String = {
    return asm.visitLabel("l" + id)
  }

  override def toString: String = "if " + _name

}