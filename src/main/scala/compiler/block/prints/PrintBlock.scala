package compiler.block.prints

import compiler.Utils
import compiler.block.Block
import compiler.symbol_table.SymbolTable

class PrintBlock(var superBlockInit: Block, var value: String, val isVariableInit: Boolean) extends Block(superBlockInit, false, false) {

  private[prints] val `type`: String = "print"
  private[prints] val _isVariable: Boolean = isVariableInit

  def init() {
  }

  def getName: String = {
    return null
  }

  def getValue: String = {
    return value
  }

  def getType: String = {
    return `type`
  }

  def getOpeningCode: String = {
    if (isVariable) {
      return "mv.visitFieldInsn(GETSTATIC, \"java/lang/System\", \"out\", \"Ljava/io/PrintStream;\");\n" + "mv.visitVarInsn(ILOAD, " + SymbolTable.getInstance.getValue(Utils.getMethod(this), value).getId + ");" + "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/io/PrintStream\", \"println\", \"(I)V\");"
    }
    else {
      //return "System.out.println(\""+value+"\");";
      return "     mv.visitFieldInsn(GETSTATIC, \"java/lang/System\", \"out\", \"Ljava/io/PrintStream;\");\n" + "            mv.visitLdcInsn(\"" + value + "\");\n" + "            mv.visitMethodInsn(INVOKEVIRTUAL, \"java/io/PrintStream\", \"println\", \"(Ljava/lang/String;)V\");"
    }
  }

  override def isVariable: Boolean = {
    return _isVariable
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "print: " + value
  }
}