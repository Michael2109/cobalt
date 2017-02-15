package test_classes.block.assignment

import test_classes.Utils
import test_classes.block.Block
import test_classes.symbol_table.SymbolTable

class AssignmentBlock(var superBlockInit: Block, declaration : Boolean, var name: String, var value: String) extends Block(superBlockInit, false, false) {

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