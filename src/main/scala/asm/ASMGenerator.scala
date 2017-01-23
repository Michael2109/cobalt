package asm

object ASMGenerator {
  private val asmGenerator: ASMGenerator = new ASMGenerator

  def getInstance: ASMGenerator = {
    return asmGenerator
  }

}

class ASMGenerator {
  // new Label
  def newLabel(name: Object): String = {
    return "Label " + name + " = new Label();\n"
  }

  // visit Label
  def visitLabel(name: Object): String = {
    return "mv.visitLabel(" + name + ");\n"
  }

  // Push a value on top of the stack
  def visitLdcInsn(value: Object): String = {
    return "mv.visitLdcInsn(" + value + ");\n"
  }

  // Store an integer in the variable table
  def visitVarInsn(id: Object): String = {
    return "mv.visitVarInsn(ISTORE," + id + ");\n"
  }


}
