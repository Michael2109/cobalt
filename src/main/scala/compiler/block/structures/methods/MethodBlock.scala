package compiler.block.structures.methods

import compiler.block.Block
import compiler.block.packages.PackageBlock
import compiler.structure.parameter.Parameter
import compiler.symbol_table.Row
import compiler.symbol_table.SymbolTable

class MethodBlock(var superBlockInit: Block, var name: String, var `type`: String, var params: Array[Parameter]) extends Block(superBlockInit, true, false) {

  private var parameterString: String = ""
  private var localVariableString: String = ""
  private var packageBlock: PackageBlock = null

  def getParameters: Array[Parameter] = {
    return params
  }

  def getName: String = name

  def getType: String = `type`

  def getValue: String = null

  def init() {

    val block: Block = superBlock.superBlock
    // Get the package the class is within
    for (fileSub <- block.subBlocks) {
      if (fileSub.isInstanceOf[PackageBlock]) {
        packageBlock = fileSub.asInstanceOf[PackageBlock]
      }
    }

    var i = 1
    for (parameter <- params) {
      parameterString += parameter.getAsmType
      Block.TOTAL_BLOCKS += 1
      localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getAsmType + "\", null, lMethod0, lMethod1, " + i + ");\n"
      SymbolTable.getInstance.addRow(new Row().setMethodName(name).setId(i).setName(parameter.getName))

      i += 1
    }

  }

  def getOpeningCode: String = {
    if (name != "main") {
      return "   {\n" + "            /* Build '" + name + "' method */\n" + "            MethodVisitor mv = cw.visitMethod(\n" + "                    ACC_PUBLIC,                         // public method\n" + "                    \"" + name + "\",                              // name\n" + "                    \"(" + parameterString + ")V\",                            // descriptor\n" + "                    null,                               // signature (null means not generic)\n" + "                    null);                              // exceptions (array of strings)\n" + "mv.visitCode();\n" + "\n" + "Label lMethod0 = new Label();\n" + "mv.visitLabel(lMethod0);\n"
    }
    else {
      return "{\n" + "// Main Method\n" + "MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, \"main\", \"([Ljava/lang/String;)V\", null, null);\n" + "mv.visitCode();\n" + "Label lMethod0 = new Label();\n" + "mv.visitLabel(lMethod0);\n"
    }
  }


  def getClosingCode: String = {
    if (name != "main") {
      return "mv.visitInsn(RETURN);     \n" +
        "Label lMethod1 = new Label();\n" +
        "mv.visitLabel(lMethod1);\n" +
        "mv.visitLocalVariable(\"this\", \"L" + packageBlock.directory + "/" + name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n               " +
        "// Return integer from top of stack\n" +
        localVariableString +
        "  mv.visitMaxs(0, 0);\n" +
        "mv.visitEnd();\n" + "}\n"
    }
    else {
      return "mv.visitInsn(RETURN);     \nLabel lMethod1 = new Label();\n" + "mv.visitLabel(lMethod1);\n" + "mv.visitLocalVariable(\"this\", \"L" + packageBlock.directory + "/" + name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n" + "mv.visitLocalVariable(\"args\", \"[Ljava/lang/String;\", null, lMethod0, lMethod1, 0);                // Return integer from top of stack\n" + localVariableString + "  mv.visitMaxs(0, 0);\n" + "mv.visitEnd();\n" + "}\n"
    }
  }


  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return name + " ( " + paramString + ")"
  }
}