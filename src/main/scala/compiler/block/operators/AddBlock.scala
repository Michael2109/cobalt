package compiler.block.operators

import compiler.Utils
import compiler.block.Block
import compiler.symbol_table.SymbolTable

class AddBlock(var superBlockInit: Block, var name: String, var valueInit: String) extends Block(superBlockInit, false, false) {
  private var `type`: String = "add"
  private var value: String = valueInit

  def init() {
    println("Getting variable.")

    println(Utils.getMethod(this) + " : " + name)
    id = (new Integer(SymbolTable.getInstance.getValue(Utils.getMethod(this), name).getId))
    println("Variable retrieved")
  }

  def getName: String = {
    return name
  }

  def setName(name: String) {
    this.name = name
  }

  def getType: String = {
    return `type`
  }

  def setType(`type`: String) {
    this.`type` = `type`
  }

  def getOpeningCode: String = {
    return "mv.visitIincInsn(" + id + ", " + this.getValue + ");"
  }

  def getValue: String = {
    return value
  }

  def setValue(value: String) {
    this.value = value
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "add: " + name
  }
}