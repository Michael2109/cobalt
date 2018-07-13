package cobalt.jar_loader

import java.io.{File, FileFilter}
import java.nio.file.{Path, Paths}
import java.util.jar.JarFile
import java.util.zip.ZipEntry

import scala.collection.mutable.ListBuffer
import scala.tools.asm.{ClassReader, Opcodes, Type}
import scala.tools.asm.tree.{ClassNode, MethodNode}


import scala.collection.JavaConversions._

object JarUtility {

  val JavaHome = Paths.get(System.getenv().get("JAVA_HOME"))

  def main(args: Array[String]): Unit = {
    println(getBytecodeClass("java.lang.Double").getMethod("toString").getMethodSignature())
  }

  def getBytecodeClass(classPath: String): BytecodeClass ={
    val cleanedClassPath = classPath.replace(".", "/") + ".class"
    findJar(JavaHome.toFile, cleanedClassPath)
  }

  private def findJar(start: File, className: String): BytecodeClass = {

    val filter = new FileFilter() {
      def accept(pathname: File): Boolean = {
        return pathname.getName().endsWith(".jar") || pathname.isDirectory()
      }
    }

    var bytecodeClass: BytecodeClass = null

    for (f <- start.listFiles(filter)) {
      if (bytecodeClass == null) {
        if (f.isDirectory()) {
          val foundBytecodeClass = findJar(f, className)
          if (foundBytecodeClass != null) {
            bytecodeClass = foundBytecodeClass
          }
        } else {
          val foundPath = searchJar(f, className)
          if (foundPath != null) {
            bytecodeClass = foundPath
          }
        }
      }
    }

    bytecodeClass
  }

  private def searchJar(f: File, className: String): BytecodeClass = {

    val jar = new JarFile(f)
    var e: ZipEntry = jar.getJarEntry(className)
    if (e == null) {
      e = jar.getJarEntry(className)
      if (e != null) {
        throw new Exception("Shouldn't reach here")
      } else {
        null
      }
    } else {

      val classNode: ClassNode = new ClassNode()
      val classFileInputStream = jar.getInputStream(e);

      val classReader = new ClassReader(classFileInputStream);
      classReader.accept(classNode, 0);

      classFileInputStream.close();

      describeClass(classNode)
    }
  }

  def describeClass(classNode: ClassNode): BytecodeClass = {
    val classDescription = new StringBuilder()

    val classType = Type.getObjectType(classNode.name)

    classDescription.append(classType.getClassName()).append("\n");
    classDescription.append("{\n");

    val methodNodes: List[MethodNode] = classNode.methods.toList
    val bytecodeMethods = methodNodes.map(describeMethod)

    BytecodeClass(classType.getDescriptor, classNode.access, bytecodeMethods)
  }

  def describeMethod(methodNode: MethodNode): BytecodeMethod ={

    val returnType = Type.getReturnType(methodNode.desc)
    val argumentTypes: List[String] = Type.getArgumentTypes(methodNode.desc).map(_.getDescriptor).toList

    BytecodeMethod(methodNode.name, methodNode.access, argumentTypes,returnType.getDescriptor, methodNode.exceptions.toList)

  }
}
