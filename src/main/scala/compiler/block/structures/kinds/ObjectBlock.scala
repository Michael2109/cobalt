package compiler.block.structures.kinds

import compiler.block.Block
import compiler.block.modifiers.ModifierBlock
import compiler.block.packages.PackageBlock
import compiler.block.structures.methods.{ConstructorBlock, MethodBlock}
import compiler.structure.parameters.Parameter
import compiler.utilities.Constants

/**
  * Represents a class.
  * Creates a constructor method. Loops through all blocks unless it's a method or within a method adding to the constructor
  */
class ObjectBlock(var superBlockInit: Block, var name: String, var parameters: Array[Parameter], parentClass: String, implementedClasses: String) extends Block(superBlockInit, true, false) {


  // Package the class is within
  private var packageBlock: PackageBlock = new PackageBlock("")

  def getName: String = name

  def getValue: String = null

  def getType: String = "class"

  /**
    * Performed just before compiling blocks to allow for action when all blocks parsed
    */
  def init() {

    val block: Block = superBlock

    // Get the package the class is within
    for (fileSub <- block.subBlocks) {
      if (fileSub.isInstanceOf[PackageBlock]) {
        packageBlock = fileSub.asInstanceOf[PackageBlock]
      }
    }

  }

  def getOpeningCode: String = {
      asm.getClassOpening(name) +
      asm.executeMethodOpening +
      asm.getClassWriter +
      asm.visitClassWriter(packageBlock.directory + "/" + name, null, parentClass, null)
  }

  def getClosingCode: String = {
     " cw.visitEnd();\n" + "return cw.toByteArray();\n" +
    asm.getClosingBrace()
    }

  override def toString: String = return "object" + name

}