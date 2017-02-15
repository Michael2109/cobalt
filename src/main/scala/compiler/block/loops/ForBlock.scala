package compiler.block.loops

import compiler.block.Block
import compiler.structure.parameters.Parameter
import test_classes.block.Block
import test_classes.structure.parameters.Parameter

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

  def getValue: String = null

  def getType: String = ""

  def getOpeningCode: String = null

  def getClosingCode: String = null

  override def toString: String = "for: " + name

}