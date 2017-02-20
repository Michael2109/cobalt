package compiler.block.modifiers

import compiler.block.Block

class ModifierBlock (var superBlockInit: Block, var value: String) extends Block(superBlockInit, true, false) {

  // Called after file is parsed
  override def init(): Unit = {}

  /* Symbol table information */
  override def getName: String = ""

  override def getValue: String = value

  override def getType: String = "modifier"

  /* Bytecode for the opening and closing of the block */
  override def getOpeningCode: String = ""

  override def getClosingCode: String = ""
}
