package compiler.block.operators

import compiler.Utils
import compiler.block.Block
import compiler.symbol_table.SymbolTable

class AddBlock(var superBlock: Block, var name: String, var valueInit: String) extends Block(superBlock, false, false) {
  private var `type`: String = "add"
  private var value: String = valueInit

  def init() {
    println("Getting variable.")

    println(Utils.getMethod(this) + " : " + name)
    setId(new Integer(SymbolTable.getInstance.getValue(Utils.getMethod(this), name).getId))
    println("Variable retrieved")
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
    return ""
  }

  def getBodyCode: String = {
    return "mv.visitLdcInsn(" + value + ");\n" +
      "mv.visitVarInsn(ILOAD," + getId + ");\n" +
      "mv.visitInsn(IADD);\n" +
      "mv.visitVarInsn(ISTORE," + getId + ");\n"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "add: " + name
  }
}