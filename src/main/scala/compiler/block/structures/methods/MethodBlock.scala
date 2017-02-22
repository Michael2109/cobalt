package compiler.block.structures.methods

import compiler.block.Block
import compiler.block.modifiers.ModifierBlock
import compiler.block.packages.PackageBlock
import compiler.generators.structures.methods.MethodGen
import compiler.structure.parameters.Parameter
import compiler.symbol_table.{Row, SymbolTable}
import compiler.utilities.Utils

class MethodBlock(var superBlockInit: Block, var name: String, var `type`: String, var params: Array[Parameter]) extends Block(superBlockInit, true, false,false) {

  private val modifier : String = {
    // Check the modifier if it exists
    if(superBlock.isInstanceOf[ModifierBlock]){
      if(superBlock.getValue == "private"){
        "ACC_PRIVATE"
      }else  if(superBlock.getValue == "public"){
        "ACC_PUBLIC"
      }else if(superBlock.getValue == "protected"){
        "ACC_PROTECTED"
      }else {
        "0"
      }
    }else{
      "0"
    }
  }

  private var parameterString: String = ""

  private var localVariableString: String = ""
  private var static: String = ""

  def getName: String = name

  def getType: String = `type`

  def getValue: String = ""

  def init() {



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

  // Package the class is within
  def packageBlock: PackageBlock = Utils.getFileBlock(this).subBlocks.find(_.isInstanceOf[PackageBlock]).getOrElse(new PackageBlock("")).asInstanceOf[PackageBlock]


  def getOpeningCode: String = {
    MethodGen.getOpeningCode(name, modifier, static, parameterString)
  }


  def getClosingCode: String = {
    MethodGen.getClosingCode(name, packageBlock.directory, localVariableString)
  }


  override def toString: String = {
    var paramString: String = ""
    for (parameter <- params) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return name + " ( " + paramString + ")"
  }
}