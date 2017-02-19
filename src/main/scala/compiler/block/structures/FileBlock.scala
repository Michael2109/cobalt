package compiler.block.structures

import java.io.File

import compiler.block.Block
import compiler.block.packages.PackageBlock
import compiler.generators.structures.FileGen
import compiler.utilities.Constants

/**
  * Represents the whole file.
  */
class FileBlock(name: String, buildDir: File) extends Block(null, true, false) {

  def init() {}

  def getName: String = name

  def getValue: String = ""

  def getType: String = "file"

  def getOpeningCode: String = {
    val packageBlock: PackageBlock = {
      var result = new PackageBlock("")
      for (sub <- subBlocks)
        if (sub.isInstanceOf[PackageBlock])
          result = sub.asInstanceOf[PackageBlock]
      result
    }
    asm.getPackage(packageBlock.directory) +
    asm.getImport("java.io.DataOutputStream") +
    asm.getImport("java.io.FileNotFoundException") +
    asm.getImport("java.io.FileOutputStream") +
    asm.getImport("java.io.IOException") +
    asm.getImport("java.io.*") +
    asm.getStaticImport("org.objectweb.asm.Opcodes.*") +
    asm.getImport("org.objectweb.asm.*")
  }

  def getClosingCode: String = {
    val packageBlock: PackageBlock = {

        var result = new PackageBlock("")
        for (sub <- subBlocks)
          if (sub.isInstanceOf[PackageBlock])
             result = sub.asInstanceOf[PackageBlock]
        result
    }
    "    public static void main(String [] args){\n   " +
      "new File(new File(\"" + buildDir.getPath + "/" + packageBlock.directory + "/" + name.split("\\.")(0) + ".class\").getParent()).mkdirs();" +
      "  DataOutputStream dout = null;\n" +
      "        try {\n" +
      "" + "            dout = new DataOutputStream(new FileOutputStream(\"" + buildDir + "/" + packageBlock.directory + "/" + name.split("\\.")(0) + ".class\"));\n" + "\n" + "        dout.write(execute());\n" + "        dout.flush();\n" + "        dout.close();\n" + "        } catch (FileNotFoundException e) {\n" + "        e.printStackTrace();\n" + "    } catch (IOException e) {\n" + "            e.printStackTrace();\n" + "        } catch (Exception e) {\n" + "            e.printStackTrace();\n" + "        " + "   } }\n" + "}"
  }

  override def toString: String = "file: " + name
}