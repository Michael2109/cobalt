package compiler.block.imports

import compiler.block.Block

class ImportBlock(var directory: String, var fileName: String) extends Block(null, false, false) {
  def init() {
  }

  def getName: String = {
    return null
  }

  def getValue: String = {
    return null
  }

  def getType: String = {
    return null
  }

  def getOpeningCode: String = {
    return null
  }

  def getClosingCode: String = {
    return null
  }

  override def toString: String = {
    return "import: " + directory + " : " + fileName
  }
}