package compiler.generators.structures.methods

import compiler.block.structures.methods.MethodBlock
import compiler.utilities.Utils

object MethodGen {

  def getOpeningCode(methodBlock: MethodBlock): String = {
    if (methodBlock.name != "main") {
      return "   {\n" + "            /* Build '" + methodBlock.name + "' method */\n" + "            MethodVisitor mv = cw.visitMethod(\n" + "                    "+methodBlock.modifier+" " +methodBlock.static+",                         // public method\n" + "                    \"" + methodBlock.name + "\",                              // name\n" + "                    \"(" + methodBlock.parameterString + ")V\",                            // descriptor\n" + "                    null,                               // signature (null means not generic)\n" + "                    null);                              // exceptions (array of strings)\n" + "mv.visitCode();\n" + "\n" + "Label lMethod0 = new Label();\n" + "mv.visitLabel(lMethod0);\n"
    }
    else {
      return "{\n" + "// Main Method\n" + "MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, \"main\", \"([Ljava/lang/String;)V\", null, null);\n" + "mv.visitCode();\n" + "Label lMethod0 = new Label();\n" + "mv.visitLabel(lMethod0);\n"
    }
  }

  def getClosingCode(methodBlock: MethodBlock): String = {
    if (methodBlock.name != "main") {
      return "mv.visitInsn(RETURN);     \n" +
        "Label lMethod1 = new Label();\n" +
        "mv.visitLabel(lMethod1);\n" +
        "mv.visitLocalVariable(\"this\", \"L" + Utils.packageBlock(methodBlock).directory + "/" + methodBlock.name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n               " +
        "// Return integer from top of stack\n" +
        methodBlock.localVariableString +
        "  mv.visitMaxs(0, 0);\n" +
        "mv.visitEnd();\n" + "}\n"
    }
    else {
      return "mv.visitInsn(RETURN);     \n" +
        "Label lMethod1 = new Label();\n" +
        "mv.visitLabel(lMethod1);\n" +
        "mv.visitLocalVariable(\"this\", \"L" +Utils.packageBlock(methodBlock).directory + "/" + methodBlock.name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n" + "mv.visitLocalVariable(\"args\", \"[Ljava/lang/String;\", null, lMethod0, lMethod1, 0);                // Return integer from top of stack\n" + methodBlock.localVariableString + "  mv.visitMaxs(0, 0);\n" + "mv.visitEnd();\n" + "}\n"
    }
  }
}