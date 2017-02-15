package test_classes.block.imports

import test_classes.block.Block

/**
  * Represents an import
  *
  * @param directory
  * @param fileName
  */
class ImportBlock(var directory: String, var fileName: String) extends Block(null, false, false) {

  def init() {}

  def getName: String = ""

  def getValue: String = ""

  def getType: String = "import"

  def getOpeningCode: String = ""

  def getClosingCode: String = ""

  override def toString: String = "import: " + directory + " : " + fileName


}