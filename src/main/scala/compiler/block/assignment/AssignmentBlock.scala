package compiler.block.assignment

import compiler.block.Block
import compiler.generators.assignment.AssignmentGen

class AssignmentBlock(var superBlockInit: Block, declaration : Boolean, var name: String, var value: String) extends Block(superBlockInit, false, false) {

  def init() {

  }

  def getName: String = name

  def getType: String = "assignment"

  def getValue: String = value

  def getOpeningCode: String = {
    return ""
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "add: " + name

}