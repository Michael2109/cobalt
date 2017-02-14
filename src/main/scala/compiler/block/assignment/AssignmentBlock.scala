package compiler.block.assignment

import compiler.Utils
import compiler.block.Block
import compiler.symbol_table.SymbolTable

class AssignmentBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, false) {

  def init() {

  }

  def getName: String = name

  def getType: String = "assignment"

  def getOpeningCode: String = {
    return ""
  }

  def getValue: String = value

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "add: " + name

}