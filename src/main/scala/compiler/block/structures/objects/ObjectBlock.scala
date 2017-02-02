package compiler.block.structures.objects

import compiler.{Parameter, Utils}
import compiler.block.Block
import compiler.block.imports.ImportBlock
import compiler.block.packages.PackageBlock
import compiler.block.structures.FileBlock
import compiler.block.structures.classes.ClassBlock
import compiler.symbol_table.SymbolTable

// Creation of a new object and storing to a variable
class ObjectBlock(var superBlockInit: Block, var className: String, var variableName: String, var operator: String, var newKeyword: String, var initClassName: String, var params: Array[Parameter]) extends Block(superBlockInit, false, true) {

  private var parameterString: String = ""
  private var argumentString: String = ""
  private[objects] var directory: String = ""

  def init() {
    if (className == getClassName) {
      directory = getPackage
    }
    else {
      directory = getDirectory
    }

    // Get the type of the parameters
    for (param <- params) {
      param.setType(SymbolTable.getInstance.getValue(Utils.getMethod(this), param.getName).getType)
      parameterString += param.getAsmType
      argumentString += "mv.visitIntInsn(ILOAD, " + SymbolTable.getInstance.getValue(Utils.getMethod(this), param.getName).getId + ");"
      println("Params" + param)
    }
    println(parameterString)
    println(argumentString)
  }

  // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
  def getDirectory: String = {
    // Get the FileBlock to find the imports
    var block: Block = this
    while (!(block.isInstanceOf[FileBlock])) {
      {
        block = block.superBlock
      }
    }
    // Get the directory of the Object
    for (sub <- block.subBlocks) {
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
        block = block.superBlock
      }
    }
    // Get the directory of the Object
    for (sub <- block.subBlocks) {
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
        block = block.superBlock
      }
    }
    // Get the directory of the Object
    return block.getName
  }

  def getName: String = variableName

  def getValue: String = null

  def getType: String = className

  def getOpeningCode: String = {
    return "mv.visitTypeInsn(NEW, \"" + directory + (if (directory == "") ""
    else "/") + className + "\");\n" + "mv.visitInsn(DUP);\n" + argumentString + "mv.visitMethodInsn(INVOKESPECIAL, \"" + directory + (if (directory == "") ""
    else "/") + className + "\", \"<init>\", \"(" + parameterString + ")V\", false);\n" + "mv.visitVarInsn(ASTORE," + id + ");\n"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = "object: " + className + " " + variableName + " = new " + initClassName + "()"

}