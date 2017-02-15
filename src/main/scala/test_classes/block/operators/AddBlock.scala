package test_classes.block.operators

import test_classes.Utils
import test_classes.block.Block
import test_classes.symbol_table.SymbolTable

/**
  * Represents adding a value to a primitive
  *
  * @param superBlockInit
  * @param name
  * @param valueInit
  */
class AddBlock(var superBlockInit: Block, var name: String, var value: String) extends Block(superBlockInit, false, false) {

  def init() {
    id = (new Integer(SymbolTable.getInstance.getValue(Utils.getMethod(this), name).getId))
  }

  def getName: String = name

  def getType: String = "add"

  def getOpeningCode: String = {
    return "mv.visitIincInsn(" + id + ", " + this.getValue + ");"
  }

  def getValue: String = value

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "add: " + name

}