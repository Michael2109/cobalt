package compiler.generators.structures.methods

object MethodGen {

  def getOpeningCode(name: String, modifier: String, static: String, parameterString: String): String = {
    if (name != "main") {
      return "   {\n" + "            /* Build '" + name + "' method */\n" + "            MethodVisitor mv = cw.visitMethod(\n" + "                    "+modifier+" " +static+",                         // public method\n" + "                    \"" + name + "\",                              // name\n" + "                    \"(" + parameterString + ")V\",                            // descriptor\n" + "                    null,                               // signature (null means not generic)\n" + "                    null);                              // exceptions (array of strings)\n" + "mv.visitCode();\n" + "\n" + "Label lMethod0 = new Label();\n" + "mv.visitLabel(lMethod0);\n"
    }
    else {
      return "{\n" + "// Main Method\n" + "MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, \"main\", \"([Ljava/lang/String;)V\", null, null);\n" + "mv.visitCode();\n" + "Label lMethod0 = new Label();\n" + "mv.visitLabel(lMethod0);\n"
    }
  }

  def getClosingCode(name: String, packageDir: String, localVariableString: String): String = {
    if (name != "main") {
      return "mv.visitInsn(RETURN);     \n" +
        "Label lMethod1 = new Label();\n" +
        "mv.visitLabel(lMethod1);\n" +
        "mv.visitLocalVariable(\"this\", \"L" + packageDir + "/" + name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n               " +
        "// Return integer from top of stack\n" +
        localVariableString +
        "  mv.visitMaxs(0, 0);\n" +
        "mv.visitEnd();\n" + "}\n"
    }
    else {
      return "mv.visitInsn(RETURN);     \nLabel lMethod1 = new Label();\n" + "mv.visitLabel(lMethod1);\n" + "mv.visitLocalVariable(\"this\", \"L" + packageDir + "/" + name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n" + "mv.visitLocalVariable(\"args\", \"[Ljava/lang/String;\", null, lMethod0, lMethod1, 0);                // Return integer from top of stack\n" + localVariableString + "  mv.visitMaxs(0, 0);\n" + "mv.visitEnd();\n" + "}\n"
    }
  }
}