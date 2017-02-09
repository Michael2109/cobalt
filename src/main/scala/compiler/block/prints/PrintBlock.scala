package compiler.block.prints

import compiler.Utils
import compiler.block.Block
import compiler.symbol_table.SymbolTable

class PrintBlock(var superBlockInit: Block, var value: String, val isVariableInit: Boolean) extends Block(superBlockInit, false, false) {

  def init() {
  }

  def getName: String = null

  def getValue: String = value

  def getType: String = "print"

  def getOpeningCode: String = {
    if (isVariableInit) {
      return "mv.visitFieldInsn(GETSTATIC, \"java/lang/System\", \"out\", \"Ljava/io/PrintStream;\");\n" +
        "mv.visitVarInsn(ALOAD, " + SymbolTable.getInstance.getValue(Utils.getMethod(this), value).getId + ");" +
        "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/io/PrintStream\", \"println\", \"(Ljava/lang/String;)V\");"
    }
    else {
      //return "System.out.println(\""+value+"\");";
      return "mv.visitFieldInsn(GETSTATIC, \"java/lang/System\", \"out\", \"Ljava/io/PrintStream;\");\n" +
        "mv.visitLdcInsn(\"" + value + "\");\n" +
        "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/io/PrintStream\", \"println\", \"(Ljava/lang/String;)V\");"
    }
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "print: " + value
}