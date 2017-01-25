package compiler.block.structures.methods

import compiler.Parameter
import compiler.block.Block

import compiler.block.packages.PackageBlock
import compiler.symbol_table.{Row, SymbolTable}

class ConstructorBlock(var superBlock: Block, var parameters: Array[Parameter]) extends Block(superBlock, true, false) {

  var parameterString = ""
  var localVariableString = ""
  private var packageBlock: PackageBlock = null

  def init(): Unit = {

    var classBlock = getSuperBlock

    var i = 1



    val block: Block = getSuperBlock.getSuperBlock
    // Get the package the class is within
    for (fileSub <- block.getSubBlocks) {
      if (fileSub.isInstanceOf[PackageBlock]) {
        packageBlock = fileSub.asInstanceOf[PackageBlock]
      }
    }
    localVariableString += "mv.visitLocalVariable(\"this\", \"L" + packageBlock.directory + "/" + classBlock.getName + ";\", null, lConstructor0, lConstructor2, " + 0 + ");\n"
    for (parameter <- parameters) {
      parameterString += parameter.getAsmType
      Block.TOTAL_BLOCKS_$eq(Block.TOTAL_BLOCKS + 1)
      localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getAsmType + "\", null, lConstructor0, lConstructor2, " + i + ");\n"
      SymbolTable.getInstance.addRow(new Row().setMethodName("constructor").setId(i).setName(parameter.getName))
      i += 1
    }
  }

  def getName: String = {
    return null
  }

  def getValue: String = {
    return null
  }

  def getType: String = {
    return null
  }

  def getOpeningCode: String = {
    return ""
  }

  def getBodyCode: String = {
    return ""
  }

  def getClosingCode: String = {
    return "mv.visitInsn(RETURN);                     " + localVariableString + " // End the constructor method\n" + "mv.visitMaxs(0, 0);\n" + "mv.visitEnd();\n" + "}"
  }

  override def toString: String = {
    var paramString: String = ""
    import scala.collection.JavaConversions._
    for (parameter <- parameters) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return "constructor: ( " + paramString + ")"
  }
}