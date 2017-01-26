package compiler.block.structures

import compiler.block.Block

/**
  * Represents the whole file.
  */
class FileBlock(nameInit: String) extends Block(null, true, false) {
  private val name: String = nameInit

  def init() {
  }

  def getName: String = {
    return name
  }

  def getValue: String = {
    return null
  }

  def getType: String = {
    return null
  }

  def getOpeningCode: String = {
    return ""
  }


  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "file: " + name
  }
}