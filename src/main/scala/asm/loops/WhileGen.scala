package compiler.block.loops

import compiler.Utils
import compiler.block.Block
import compiler.symbol_table.SymbolTable

class WhileGen(var superBlockInit: Block, var name: String) extends Block(superBlockInit, true, false) {

  val split: Array[String] = name.split(" ")
  private var pointer: String = null
  private var operator: String = null
  private var value: String = null
  private var byteCodeOp: String = ""

  def getName: String = name

  def getValue: String = null

  def getType: String = "while"

  def init(): Unit = {

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
  }

  def getOpeningCode: String = {
    return "Label start" + id + " = new Label();\n" +
      "mv.visitLabel(start" + id + ");\n" +
      "mv.visitVarInsn(ILOAD," + pointer + ");\n" +
      "mv.visitLdcInsn(" + value + ");\n" +
      "Label l" + id + " = new Label();\n" +
      byteCodeOp
  }

  def getClosingCode: String = {
    return "mv.visitJumpInsn(GOTO, start" + id + ");\n" + "mv.visitLabel(l" + id + ");\n"
  }

  override def toString: String = "while: " + name

}