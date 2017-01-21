package compiler.block.structures.objects

import compiler.Parameter
import compiler.Utils
import compiler.block.Block
import compiler.block.imports.ImportBlock
import compiler.block.packages.PackageBlock
import compiler.block.structures.FileBlock
import compiler.block.structures.classes.ClassBlock
import compiler.symbol_table.SymbolTable

/**
  * Calling a method of an object
  */
class ObjectMethodCallBlock(var superBlock: Block, var variableName: String, var methodName: String, var params: Array[Parameter]) extends Block(superBlock, false, false) {
  setId(SymbolTable.getInstance.getValue(Utils.getMethod(this), variableName).getId)
  className = SymbolTable.getInstance.getValue(Utils.getMethod(this), variableName).getType
  private val `type`: String = null
  private[objects] var parameterString: String = ""
  private[objects] var argumentString: String = ""
  private var className: String = null
  private var directory: String = ""

  def getParameters: Array[Parameter] = {
    return params
  }

  def getType: String = {
    return `type`
  }

  def getClosingCode: String = {
    return ""
  }

  def getValue: String = {
    return null
  }

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

  def getOpeningCode: String = {
    return ""
  }

  def getBodyCode: String = {
    // Push all arguments to the top of the stack
    //
    return "mv.visitVarInsn(ALOAD, " + getId + ");\n" + argumentString + "mv.visitMethodInsn(INVOKEVIRTUAL, \"" + directory + "/" + className + "\", \"" + methodName + "\", \"(" + parameterString + ")V\", false);\n"
  }

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return "object method call: " + variableName + " ( " + paramString + ")"
  }
}