package test_classes.block.comments

import test_classes.block.Block

/**
  * Represents a code comment
  * E.g. "// This is a comment"
  *
  * @param superBlockInit
  */
class CommentBlock(var superBlockInit: Block) extends Block(superBlockInit, false, false) {

  // Called after file is parsed
  override def init() {}

  /* Symbol table information */
  override def getName: String = ""

  override def getValue: String = ""

  override def getType: String = "comment"

  /* Opening and closing byte code */
  def getOpeningCode: String = ""

  def getClosingCode: String = ""

  /* toString method */
  override def toString: String = "Comment"

}