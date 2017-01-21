package compiler.block.loops

import compiler.Parameter
import compiler.block.Block

class ForBlock(var superBlock: Block, var name: String) extends Block(superBlock, true, false) {
  private val `type`: String = "for"
  private val params: Array[Parameter] = null

  def getParameters: Array[Parameter] = {
    return params
  }

  def init() {
  }

  def getName: String = {
    return name
  }

  def getValue: String = {
    return null
  }

  def getType: String = {
    return `type`
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
    return "for: " + name
  }
}