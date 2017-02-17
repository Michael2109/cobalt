package compiler.block.structures.methods

import compiler.Utils
import compiler.block.Block
import compiler.block.modifiers.ModifierBlock
import compiler.block.packages.PackageBlock
import compiler.generators.Generator
import compiler.structure.parameters.Parameter
import compiler.symbol_table.{Row, SymbolTable}

class MethodBlock(var superBlockInit: Block, var name: String, var `type`: String, var params: Array[Parameter]) extends Block(superBlockInit, true, false,false) {

  private var modifier : String = "0"
  private var parameterString: String = ""
  private var localVariableString: String = ""
  private var packageBlock: PackageBlock = null
  private var static: String = ""


  def getParameters: Array[Parameter] = {
    return params
  }

  def getName: String = name

  def getType: String = `type`

  def getValue: String = ""

  def init() {


      // Get the package the class is within
      for (fileSub <- Utils.getFileBlock(superBlock).subBlocks) {
        if (fileSub.isInstanceOf[PackageBlock]) {
          packageBlock = fileSub.asInstanceOf[PackageBlock]
        }
      }

    // Check the modifier if it exists
    if(superBlock.isInstanceOf[ModifierBlock]){
      if(superBlock.getValue == "private"){
        modifier = "ACC_PRIVATE"
      }else  if(superBlock.getValue == "public"){
        modifier = "ACC_PUBLIC"
      }else if(superBlock.getValue == "protected"){
        modifier = "ACC_PROTECTED"
      }
    }

    if(!Utils.isClass(this)){
      static = "+ACC_STATIC"
    }


    var i = 1
    for (parameter <- params) {
      parameterString += parameter.getAsmType
      Block.TOTAL_BLOCKS += 1
      localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getAsmType + "\", null, lMethod0, lMethod1, " + i + ");\n"
      SymbolTable.getInstance.addRow(new Row().setMethodName(name).setId(i).setName(parameter.getName))

      i += 1
    }

  }

  def getOpeningCode: String = {
    Generator.METHOD_GEN.getOpeningCode(name, modifier, static, parameterString)
  }


  def getClosingCode: String = {
    Generator.METHOD_GEN.getClosingCode(name, packageBlock.directory, localVariableString)
  }


  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return name + " ( " + paramString + ")"
  }
}