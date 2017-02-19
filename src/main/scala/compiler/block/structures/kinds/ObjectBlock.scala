package compiler.block.structures.kinds

import compiler.Constants
import compiler.block.Block
import compiler.block.modifiers.ModifierBlock
import compiler.block.packages.PackageBlock
import compiler.block.structures.methods.{ConstructorBlock, MethodBlock}
import compiler.structure.parameters.Parameter

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
    return asm.getPackage(packageBlock.directory) +
      asm.getImport("java.io.DataOutputStream") +
      asm.getImport("java.io.FileNotFoundException") +
      asm.getImport("java.io.FileOutputStream") +
      asm.getImport("java.io.IOException") +
      asm.getImport("java.io.*") +
      asm.getStaticImport("org.objectweb.asm.Opcodes.*") +
      asm.getImport("org.objectweb.asm.*") +
      asm.getClassOpening(name) +
      asm.executeMethodOpening +
      asm.getClassWriter +
      asm.visitClassWriter(packageBlock.directory + "/" + name, null, parentClass, null)
  }

  def getClosingCode: String = {
    return " cw.visitEnd();\n" + "return cw.toByteArray();}\n" +
      "    public static void main(String [] args){\n   " +
      "new File(new File(\""+Constants.BUILD_DIR+"/" + packageBlock.directory + "/" + name + ".class\").getParent()).mkdirs();"+
      "  DataOutputStream dout = null;\n" +
      "        try {\n" +
      "" + "            dout = new DataOutputStream(new FileOutputStream(\""+Constants.BUILD_DIR + "/" +packageBlock.directory + "/" + name + ".class\"));\n" + "\n" + "        dout.write(execute());\n" + "        dout.flush();\n" + "        dout.close();\n" + "        } catch (FileNotFoundException e) {\n" + "        e.printStackTrace();\n" + "    } catch (IOException e) {\n" + "            e.printStackTrace();\n" + "        } catch (Exception e) {\n" + "            e.printStackTrace();\n" + "        " + "   } }\n" + "}"
  }

  override def toString: String = return "object" + name

}