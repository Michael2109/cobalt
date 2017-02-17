package compiler.block.structures

import compiler.block.Block
import compiler.generators.structures.FileGen

/**
  * Represents the whole file.
  */
class FileBlock(name: String) extends Block(null, true, false) {

  def init() {}

  def getName: String = name

  def getValue: String = ""

  def getType: String = "file"

  def getOpeningCode: String = ""

  def getClosingCode: String = ""

  override def toString: String = "file: " + name
}