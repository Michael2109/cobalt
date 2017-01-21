package compiler.block.structures.objects

import compiler.block.Block
import compiler.block.imports.ImportBlock
import compiler.block.packages.PackageBlock
import compiler.block.structures.FileBlock
import compiler.block.structures.classes.ClassBlock

// Creation of a new object and storing to a variable
class ObjectBlock(var superBlock: Block, var className: String, var variableName: String, var operator: String, var newKeyword: String, var initClassName: String) extends Block(superBlock, false, true) {

  private[objects] var directory: String = ""

  def init() {
    if (className == getClassName) {
      directory = getPackage
    }
    else {
      directory = getDirectory
    }
  }

  // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
  def getDirectory: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!(block.isInstanceOf[FileBlock])) {
      {
        block = block.getSuperBlock
      }
    }
    // Get the directory of the Object
    for (sub <- block.getSubBlocks) {
      if (sub.isInstanceOf[ImportBlock] && (sub.asInstanceOf[ImportBlock]).fileName == className) {
        return (sub.asInstanceOf[ImportBlock]).directory
      }
    }
    return ""
  }

  // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
  def getPackage: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!(block.isInstanceOf[FileBlock])) {
      {
        block = block.getSuperBlock
      }
    }
    // Get the directory of the Object
    for (sub <- block.getSubBlocks) {
      if (sub.isInstanceOf[PackageBlock]) {
        return (sub.asInstanceOf[PackageBlock]).directory
      }
    }
    return ""
  }

  // Returns the main class name for the file
  def getClassName: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!(block.isInstanceOf[ClassBlock])) {
      {
        block = block.getSuperBlock
      }
    }
    // Get the directory of the Object
    return block.getName
  }

  def getName: String = {
    return variableName
  }

  def getValue: String = {
    return null
  }

  def getType: String = {
    return className
  }

  def getOpeningCode: String = {
    return ""
  }

  def getBodyCode: String = {
    return "mv.visitTypeInsn(NEW, \"" + directory + (if (directory == "") ""
    else "/") + className + "\");\n" + "mv.visitInsn(DUP);\n" + "mv.visitMethodInsn(INVOKESPECIAL, \"" + directory + (if (directory == "") ""
    else "/") + className + "\", \"<init>\", \"()V\", false);\n" + "mv.visitVarInsn(ASTORE," + getId + ");\n"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    return "object: " + className + " " + variableName + " = new " + initClassName + "()"
  }
}