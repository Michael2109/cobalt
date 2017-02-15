package test_classes.block.structures.methods

import test_classes.block.Block
import test_classes.block.packages.PackageBlock
import test_classes.structure.parameters.Parameter
import test_classes.symbol_table.{Row, SymbolTable}

class ConstructorBlock(var superBlockInit: Block, var parameters: Array[Parameter], className: String) extends Block(superBlockInit, true, false) {

  var parameterString = ""
  var localVariableString = ""
  private var packageBlock: PackageBlock = null

  def init(): Unit = {

    val classBlock = superBlock

    var i = 1

    val block: Block = superBlock.superBlock
    // Get the package the class is within
    for (fileSub <- block.subBlocks) {
      if (fileSub.isInstanceOf[PackageBlock]) {
        packageBlock = fileSub.asInstanceOf[PackageBlock]
      }
    }
    localVariableString += "mv.visitLocalVariable(\"this\", \"L" + packageBlock.directory + "/" + classBlock.getName + ";\", null, lConstructor0, lConstructor2, " + 0 + ");\n"
    for (parameter <- parameters) {
      parameterString += parameter.getAsmType
      Block.TOTAL_BLOCKS_$eq(Block.TOTAL_BLOCKS + 1)
      localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getAsmType + "\", null, lConstructor0, lConstructor2, " + i + ");\n"
      SymbolTable.getInstance.addRow(new Row().setId(i).setName(parameter.getName).setClassName(className))
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
    return "constructor"
  }

  def getOpeningCode: String = {
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