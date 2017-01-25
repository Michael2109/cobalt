package compiler.block.structures.classes

import compiler.Parameter
import compiler.block.Block
import compiler.block.packages.PackageBlock
import compiler.block.structures.methods.ConstructorBlock
import compiler.block.structures.methods.MethodBlock
import java.util.List

/**
  * Represents a class.
  * Creates a constructor method. Loops through all blocks unless it's a method or within a method adding to the constructor
  */
class ClassBlock(var superBlock: Block, var name: String, var parameters: List[Parameter]) // todo Add local variables to the symbol table
  extends Block(superBlock, true, false) {

  import scala.collection.JavaConversions._

  // Parameters added to constuctor
  var parameterString: String = ""
  // Local variables from the parameters
  var localVariableString: String = ""

  // Loop through all code that isn't a method or is within a method and move it into the constructor block

  // Package the class is within
  private[classes] var packageBlock: PackageBlock = new PackageBlock("")
  private[classes] var constructorBlock: Block = new ConstructorBlock(this, parameters.toArray(new Array[Parameter](parameters.size)))

  addBlock(constructorBlock)

  def getType: String = {
    return null
  }

  def getClosingCode: String = {
    return " cw.visitEnd();\n" + "return cw.toByteArray();}\n" +
      "    public static void main(String [] args){\n   " +
      "  DataOutputStream dout = null;\n" +
      "        try {\n" +
      "" + "            dout = new DataOutputStream(new FileOutputStream(\"build/classes/main/" + packageBlock.directory + "/" + name + ".class\"));\n" + "\n" + "        dout.write(dump());\n" + "        dout.flush();\n" + "        dout.close();\n" + "        } catch (FileNotFoundException e) {\n" + "        e.printStackTrace();\n" + "    } catch (IOException e) {\n" + "            e.printStackTrace();\n" + "        } catch (Exception e) {\n" + "            e.printStackTrace();\n" + "        " + "   } }\n" + "}"
  }

  def getValue: String = {
    return null
  }

  /**
    * Performed just before compiling blocks to allow for action when all blocks parsed
    */
  def init() {
    // Move anything outside a method and within the class to a constructor block
    for (sub <- getSubBlocks) {
      moveToConstructor(sub)
    }
    val block: Block = getSuperBlock
    // Get the package the class is within
    for (fileSub <- block.getSubBlocks) {
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

  def getName: String = {
    return name
  }

  def getOpeningCode: String = {
    return "package " + packageBlock.directory + ";\n" +
      "import java.io.DataOutputStream;\n" +
      "import java.io.FileNotFoundException;\n" +
      "import java.io.FileOutputStream;\n" +
      "import java.io.IOException;\n" +
      "\n" +
      "import static org.objectweb.asm.Opcodes.*;\n" +
      "import org.objectweb.asm.*;\n" +
      "\n" + "public class " + name + " {\n" + "\n" +
      "    public static byte[] dump() throws Exception {\n" + "\n" +
      "        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS); \n\n  " +
      "  // Visit the class itself\n" + "        {\n" + "            cw.visit(V1_7,                              // Java 1.7\n" +
      "                    ACC_PUBLIC,                         // public class\n" + "                    \"" + packageBlock.directory + "/" + name + "\",    // package and name\n" +
      "                    null,                               // signature (null means not generic)\n" +
      "                    \"java/lang/Object\",                 // superclass\n" + "                    new String[]{}); // interfaces\n" +
      " }" + "" + "\n\n\n// Build the constructor\n" + "        {\n" + "            MethodVisitor mv = cw.visitMethod(\n" + "                    ACC_PUBLIC,                         // public method\n" +
      "                    \"<init>\",                           // method name\n" + "                    \"(" + parameterString + ")V\",                              // descriptor\n" + "                    null,                               // signature (null means not generic)\n" + "                    null);                              // exceptions (array of strings)\n" + "\n" + "            mv.visitCode();                            // Start the code for this method\n" + " Label lConstructor0 = new Label();\n" + "mv.visitLabel(lConstructor0);\n" + "            mv.visitVarInsn(ALOAD, 0);                 // Load \"this\" onto the stack\n" + "\n" + "            mv.visitMethodInsn(INVOKESPECIAL,          // Invoke an instance method (non-virtual)\n" + "                    \"java/lang/Object\",                 // Class on which the method is defined\n" + "                    \"<init>\",                           // Name of the method\n" + "                    \"()V\",                              // Descriptor\n" + "                    false);                             // Is this class an interface?\n" + "\n" + "Label lConstructor2 = new Label();\n" + "mv.visitLabel(lConstructor2);\n" + "\n" + "       "
  }

  def getBodyCode: String = {
    return ""
  }

  // Moves all blocks that are inside the class and outside methods into the constructor block
  def moveToConstructor(block: Block) {
    if (block.isInstanceOf[MethodBlock] || block.isInstanceOf[ConstructorBlock]) {
      return
    }
    else {
      val ref: Block = block
      constructorBlock.addBlock(ref)
      block.getSuperBlock.removeBlock(block)
      block.setSuperBlock(constructorBlock)
    }
    for (sub <- block.getSubBlocks) {
      moveToConstructor(sub)
    }
  }

  override def toString: String = {
    var paramString: String = ""
    import scala.collection.JavaConversions._
    for (parameter <- parameters) {
      paramString += parameter.getType + ":" + parameter.getName + "; "
    }
    return name + " ( " + paramString + ")"
  }
}