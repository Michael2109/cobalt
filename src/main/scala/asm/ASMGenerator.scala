package asm

object ASMGenerator {

  // new Label
  def getLabel(name: String): String = {
    return "Label " + name + " = new Label();"
  }

  // visit Label
  def visitLabel(name: String): Unit = {
    return "mv.visitLabel(" + name + ");"
  }


}
