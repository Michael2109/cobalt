package compiler.block.packages

import compiler.block.Block

class PackageBlock(var directory: String) extends Block(null, false, false) {
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

  def getBodyCode: String = {
    return null
  }

  def getClosingCode: String = {
    return null
  }

  override def toString: String = {
    return "package: " + directory
  }
}