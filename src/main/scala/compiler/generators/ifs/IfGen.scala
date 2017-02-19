package compiler.generators.ifs

import compiler.generators.Generator

object IfGen extends Generator{

  def getOpeningCode(pointer : String, value : String, id: Int, byteCodeOp : String) :String = {
    asm.visitVarInsn("ILOAD", pointer) +
    asm.visitLdcInsn(value) +
    asm.newLabel("l" + id) +
    byteCodeOp
  }

  def getClosingCode(id : Int) : String = {
    asm.visitLabel("l" + id)
  }

}