package compiler.block.comments

import compiler.block.Block

class CommentBlock(var superBlock: Block) extends Block(superBlock, false, false) {
  private val `type`: String = "comment"

  def init() {
  }

  def getName: String = {
    return null
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
    return "Comment"
  }
}