package compiler.block.structures.methods

import compiler.Parameter
import compiler.block.Block
import compiler.block.packages.PackageBlock
import compiler.symbol_table.Row
import compiler.symbol_table.SymbolTable

class MethodBlock(var superBlock: Block, var name: String, var `type`: String, var params: Array[Parameter]) extends Block(superBlock, true, false) {

  private var parameterString: String = ""
  private var localVariableString: String = ""
  private var packageBlock: PackageBlock = null


  def getParameters: Array[Parameter] = {
    return params
  }

  def getType: String = {
    return `type`
  }


  def getValue: String = {
    return null
  }

  def init() {


    val block: Block = getSuperBlock.getSuperBlock
    // Get the package the class is within
    for (fileSub <- block.getSubBlocks) {
      if (fileSub.isInstanceOf[PackageBlock]) {
        packageBlock = fileSub.asInstanceOf[PackageBlock]
      }
    }

    var i = 1
    for (parameter <- params) {
      parameterString += parameter.getAsmType
      Block.TOTAL_BLOCKS_$eq(Block.TOTAL_BLOCKS + 1)
      localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getAsmType + "\", null, lMethod0, lMethod1, " + i + ");\n"
      SymbolTable.getInstance.addRow(new Row().setMethodName(name).setId(i).setName(parameter.getName))
      i += 1
    }

  }

  def getName: String = {
    return name
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
      print("Isn't a main method")
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
      print("Main method!!!!!!")
      return "mv.visitInsn(RETURN);     \nLabel lMethod1 = new Label();\n" + "mv.visitLabel(lMethod1);\n" + "mv.visitLocalVariable(\"this\", \"L" + packageBlock.directory + "/" + name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n" + "mv.visitLocalVariable(\"args\", \"[Ljava/lang/String;\", null, lMethod0, lMethod1, 0);                // Return integer from top of stack\n" + localVariableString + "  mv.visitMaxs(0, 0);\n" + "mv.visitEnd();\n" + "}\n"
    }
  }

  def getBodyCode: String = {
    return ""
  }

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return name + " ( " + paramString + ")"
  }
}