package compiler.generators.loops

import compiler.generators.Generator

object WhileGen extends Generator {

  def getOpeningCode(id: Int, pointer: String, value: String, byteCodeOp: String): String = {
    asm.newLabel("start" + id) +
      asm.visitLabel("start" + id) +
      asm.visitVarInsn("ILOAD", pointer) +
      asm.visitLdcInsn(value) +
      asm.newLabel("l" + id) +
    byteCodeOp
  }

  def getClosingCode(id: Int): String = {
    "mv.visitJumpInsn(GOTO, start" + id + ");\n" +
      "mv.visitLabel(l" + id + ");\n"
  }

}