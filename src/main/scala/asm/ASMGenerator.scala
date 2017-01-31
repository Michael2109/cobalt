package asm

class ASMGenerator {

  // opening brace "{"
  def getOpeningBrace() = "\n{\n"

  // closing brace "}"
  def getClosingBrace() = "\n}\n"

  // Comments
  def getComment(comment: String) = "// " + comment + "\n"

  // package
  def getPackage(name: String): String = "package " + name + ";\n"

  // import
  def getImport(location: String): String = "import " + location + ";\n"

  // import static
  def getStaticImport(location: String): String = "import static " + location + ";\n"

  // class opening
  def getClassOpening(name: String): String = "\npublic class " + name + "{\n"

  // execute method
  def executeMethodOpening: String = "public static byte[] execute() throws Exception {\n"

  // class writer
  def getClassWriter: String = "ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);\n"

  // visit class writer
  def visitClassWriter(classDir: String, signature: String, superClass: String, interfaces: List[String]): String = {
    return "cw.visit(V1_7, ACC_PUBLIC, \"" + classDir + "\", " + signature + ", \"" + superClass + "\", new String[]{});\n"
  }

  // Method Visitor
  def getMethodVisitor(name: String, descriptor: String, signature: String, exceptions: String): String = {
    return "MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, \"" + name + "\", \"" + descriptor + "\" ," + signature + ", null);\n"
  }

  // Visit Code
  def visitCode(): String = "mv.visitCode();\n"

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
  def visitVarInsn(operation: String, id: Object): String = {
    return "mv.visitVarInsn(" + operation + "," + id + ");\n"
  }


}
