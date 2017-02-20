package compiler.block.loops

import compiler.block.Block
import compiler.structure.parameters.Parameter

/**
  * Represents a for loop
  *
  * @param superBlockInit
  * @param name
  */
class ForBlock(var superBlockInit: Block, var name: String) extends Block(superBlockInit, true, false) {

  private val params: Array[Parameter] = null

  def getParameters: Array[Parameter] = {
    return params
  }

  def init() {
  }

  def getName: String = name

  def getValue: String = ""

  def getType: String = ""

  def getOpeningCode: String = ""

  def getClosingCode: String = ""

  override def toString: String = "for: " + name

}