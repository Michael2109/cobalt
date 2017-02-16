package compiler.block.structures

import compiler.Utils
import compiler.block.Block
import compiler.block.imports.ImportBlock
import compiler.block.packages.PackageBlock
import compiler.block.structures.kinds.{ClassBlock, ObjectBlock}
import compiler.structure.parameters.Parameter
import compiler.symbol_table.SymbolTable

/**
  * Calling a method of an object
  */
class ObjectMethodCallBlock(var superBlockInit: Block, var variableName: String, var methodName: String, var params: Array[Parameter]) extends Block(superBlockInit, false, false) {

  id_=(SymbolTable.getInstance.getValue(Utils.getMethod(this), variableName).getId)
  private val `type`: String = null
  private val className: String = SymbolTable.getInstance.getValue(Utils.getMethod(this), variableName).getType
  private var parameterString: String = ""
  private var argumentString: String = ""
  private var directory: String = ""

  def getParameters: Array[Parameter] = {
    return params
  }

  def getName: String = variableName

  def getValue: String = ""

  def getType: String = `type`

  def init() {
    if (className == getClassName)
      directory = getPackage
    else
      directory = getDirectory

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
    while (!(block.isInstanceOf[ClassBlock]) && !(block.isInstanceOf[ObjectBlock])) {
      {
        block = block.superBlock
      }
    }

    // Get the directory of the Object
    return block.getName
  }


  def getOpeningCode: String = {
    return "mv.visitVarInsn(ALOAD, " + id + ");\n" + argumentString + "mv.visitMethodInsn(INVOKEVIRTUAL, \"" + directory + "/" + className + "\", \"" + methodName + "\", \"(" + parameterString + ")V\", false);\n"
  }

  def getClosingCode: String = {
    return ""
  }

  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return "object method call: " + variableName + " ( " + paramString + ")"
  }
}