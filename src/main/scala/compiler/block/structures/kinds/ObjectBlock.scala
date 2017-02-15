package compiler.block.structures.kinds

import compiler.block.Block
import compiler.block.modifiers.ModifierBlock
import compiler.block.packages.PackageBlock
import compiler.block.structures.methods.{ConstructorBlock, MethodBlock}
import compiler.structure.parameters.Parameter
import test_classes.block.Block
import test_classes.block.modifiers.ModifierBlock
import test_classes.block.packages.PackageBlock
import test_classes.block.structures.methods.{ConstructorBlock, MethodBlock}

/**
  * Represents a class.
  * Creates a constructor method. Loops through all blocks unless it's a method or within a method adding to the constructor
  */
class ObjectBlock(var superBlockInit: Block, var name: String, var parameters: Array[Parameter], parentClass: String, implementedClasses: String) extends Block(superBlockInit, true, false) {



  // Package the class is within
  private var packageBlock: PackageBlock = new PackageBlock("")

  // Parameters added to constuctor
  private var parameterString: String = ""

  // Local variables from the parameters
  private var localVariableString: String = ""

  // Create a constructor block and add it to the class block
  private var constructorBlock: Block = new ConstructorBlock(this, parameters, name)
  addBlock_=(constructorBlock)


  def getName: String = name

  def getValue: String = null

  def getType: String = "class"

  /**
    * Performed just before compiling blocks to allow for action when all blocks parsed
    */
  def init() {
    // Move anything outside a method and within the class to a constructor block
    for (sub <- subBlocks) {
      moveToConstructor(sub)
    }
    val block: Block = superBlock

    // Get the package the class is within
    for (fileSub <- block.subBlocks) {
      if (fileSub.isInstanceOf[PackageBlock]) {
        packageBlock = fileSub.asInstanceOf[PackageBlock]
      }
    }

    for (parameter <- parameters) {
      parameterString += parameter.getAsmType
      Block.TOTAL_BLOCKS_$eq(Block.TOTAL_BLOCKS + 1)
      localVariableString += "mv.visitLocalVariable(\"" + parameter.getName + "\", \"" + parameter.getAsmType + "\", null, lConstructor0, lConstructor2, " + Block.TOTAL_BLOCKS + ");\n"
    }
  }

  // Moves all blocks that are inside the class and outside methods into the constructor block
  def moveToConstructor(block: Block) {
    if (block.isInstanceOf[MethodBlock] || block.isInstanceOf[ConstructorBlock] || block.isInstanceOf[ModifierBlock]) {
      return
    }
    else {
      val ref: Block = block
      constructorBlock.addBlock_=(ref)
      block.superBlock.removeBlock_=(block)
      block.superBlock_=(constructorBlock)
    }
    for (sub <- block.subBlocks) {
      moveToConstructor(sub)
    }
  }

  def getOpeningCode: String = {
    return asm.getPackage(packageBlock.directory) +
      asm.getImport("java.io.DataOutputStream") +
      asm.getImport("java.io.FileNotFoundException") +
      asm.getImport("java.io.FileOutputStream") +
      asm.getImport("java.io.IOException") +
      asm.getStaticImport("org.objectweb.asm.Opcodes.*") +
      asm.getImport("org.objectweb.asm.*") +
      asm.getClassOpening(name) +
      asm.executeMethodOpening +
      asm.getClassWriter +
      asm.visitClassWriter(packageBlock.directory + "/" + name, null, parentClass, null) +
      asm.getOpeningBrace()

  }

  def getClosingCode: String = {
    return " cw.visitEnd();\n" + "return cw.toByteArray();}\n" +
      "    public static void main(String [] args){\n   " +
      "  DataOutputStream dout = null;\n" +
      "        try {\n" +
      "" + "            dout = new DataOutputStream(new FileOutputStream(\"build/classes/main/" + packageBlock.directory + "/" + name + ".class\"));\n" + "\n" + "        dout.write(execute());\n" + "        dout.flush();\n" + "        dout.close();\n" + "        } catch (FileNotFoundException e) {\n" + "        e.printStackTrace();\n" + "    } catch (IOException e) {\n" + "            e.printStackTrace();\n" + "        } catch (Exception e) {\n" + "            e.printStackTrace();\n" + "        " + "   } }\n" + "}"
  }


  override def toString: String = {
    var paramString: String = ""
    for (parameter <- parameters) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return name + " ( " + paramString + ") extends " + parentClass + " implements " + implementedClasses
  }
}