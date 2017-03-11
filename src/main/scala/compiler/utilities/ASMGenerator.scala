/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Cobalt
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package compiler.utilities

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
  def visitClassWriter(modifiers: String, classDir: String, signature: String, superClass: String, interfaces: List[String]): String = "cw.visit(V1_7, ACC_PUBLIC " + modifiers + ", \"" + classDir + "\", " + signature + ", \"" + superClass + "\", new String[]{});\n"


  // Method Visitor
  def getMethodVisitor(name: String, descriptor: String, signature: String, exceptions: String): String = "MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, \"" + name + "\", \"" + descriptor + "\" ," + signature + ", null);\n"


  // Visit Code
  def visitCode(): String = "mv.visitCode();\n"

  // new Label
  def newLabel(name: Object): String = "Label " + name + " = new Label();\n"

  // visit Label
  def visitLabel(name: Object): String = "mv.visitLabel(" + name + ");\n"

  // Push a value on top of the stack
  def visitLdcInsn(value: Object): String = "mv.visitLdcInsn(" + value + ");\n"

  def visitInsn(value: Object): String = "mv.visitInsn(" + value + ");"

  def visitIntInsn(op: String, value: Object): String = "mv.visitIntInsn(" + op + "," + value + ");"


  // Perform an operation
  def visitVarInsn(operation: String, id: Object): String = "mv.visitVarInsn(" + operation + "," + id + ");\n"

  // Visit Jump todo debug this as generated code created an if statement no a while for WhileBlock
  def visitJumpInsn(operation: String, id: Object): String = "mv.visitJumpInsn(" + operation + "," + id + ");\n"


}
