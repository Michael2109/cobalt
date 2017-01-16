package compiler.block.structures.classes;

import compiler.Parameter;
import compiler.block.Block;
import compiler.block.packages.PackageBlock;
import compiler.block.structures.methods.ConstructorBlock;
import compiler.block.structures.methods.MethodBlock;

import java.util.List;

/**
 * Represents a class.
 * Creates a constructor method. Loops through all blocks unless it's a method or within a method adding to the constructor
 */
public class ClassBlock extends Block {
	
	private String name;
	private List<Parameter> parameters;

	// Parameters added to constuctor
	String parameterString = "";

	// Local variables from the parameters
	String localVariableString = "";

	// Package the class is within
	PackageBlock packageBlock = new PackageBlock("");

    Block constructorBlock;

	public ClassBlock(Block superBlock, String name, List<Parameter> parameters) {
		super(superBlock, true, false);
		this.name = name;
		this.parameters = parameters;

		for(Parameter parameter : parameters){

			parameterString += parameter.getAsmType();

			Block.TOTAL_BLOCKS++;
			localVariableString += "mv.visitLocalVariable(\""+parameter.getName()+"\", \""+parameter.getAsmType()+"\", null, lConstructor0, lConstructor2, "+Block.TOTAL_BLOCKS+");\n";
		}
		constructorBlock = new ConstructorBlock(this, parameters);

        // Loop through all code that isn't a method or is within a method and move it into the constructor block
        addBlock(constructorBlock);



		// todo Add local variables to the symbol table
	}
	
	public String getName() {
		return name;
	}

	@Override
	public String getValue() {
		return null;
	}

	@Override
	public String getType() {
		return null;
	}

	@Override
	public String getOpeningCode() {
		return   "package "+packageBlock.directory+";\n" +
				"\n" +
				"import java.io.DataOutputStream;\n" +
				"import java.io.FileNotFoundException;\n" +
				"import java.io.FileOutputStream;\n" +
				"import java.io.IOException;\n" +
				"\n" +
				"import static org.objectweb.asm.Opcodes.*;\n" +
				"import org.objectweb.asm.*;\n" +
				"\n" +
				"public class "+name+" {\n" +
				"\n" +
				"    public static byte[] dump() throws Exception {\n" +
				"\n" +
				"        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES); \n\n  " +
				"  // Visit the class itself\n" +
				"        {\n" +
				"            cw.visit(V1_7,                              // Java 1.7\n" +
				"                    ACC_PUBLIC,                         // public class\n" +
				"                    \""+packageBlock.directory+"/"+name+"\",    // package and name\n" +
				"                    null,                               // signature (null means not generic)\n" +
				"                    \"java/lang/Object\",                 // superclass\n" +
				"                    new String[]{}); // interfaces\n" +
				"        }" +
				"" +
				"\n\n\n// Build the constructor\n" +
				"        {\n" +
				"            MethodVisitor mv = cw.visitMethod(\n" +
				"                    ACC_PUBLIC,                         // public method\n" +
				"                    \"<init>\",                           // method name\n" +
				"                    \"("+parameterString+")V\",                              // descriptor\n" +
				"                    null,                               // signature (null means not generic)\n" +
				"                    null);                              // exceptions (array of strings)\n" +
				"\n" +
				"            mv.visitCode();                            // Start the code for this method\n" +
				" Label lConstructor0 = new Label();\n" +
				"mv.visitLabel(lConstructor0);\n" +
				"            mv.visitVarInsn(ALOAD, 0);                 // Load \"this\" onto the stack\n" +
				"\n" +
				"            mv.visitMethodInsn(INVOKESPECIAL,          // Invoke an instance method (non-virtual)\n" +
				"                    \"java/lang/Object\",                 // Class on which the method is defined\n" +
				"                    \"<init>\",                           // Name of the method\n" +
				"                    \"()V\",                              // Descriptor\n" +
				"                    false);                             // Is this class an interface?\n" +
				"\n" +

				"Label lConstructor2 = new Label();\n" +
				"mv.visitLabel(lConstructor2);\n" +
				"mv.visitLocalVariable(\"this\", \"L"+packageBlock.directory+"/"+name+";\", null, lConstructor0, lConstructor2, "+id+");\n" +
				localVariableString + "\n"+
				"       ";

	}

	@Override
	public String getBodyCode() {
		return "";
	}

	@Override
	public String getClosingCode() {
		return "\n" +
                "return cw.toByteArray();}\n" +
				"    public static void main(String [] args){\n   " +
				"  DataOutputStream dout = null;\n" +
				"        try {\n" +
				"            dout = new DataOutputStream(new FileOutputStream(\"build/classes/main/"+packageBlock.directory+"/"+name+".class\"));\n" +
				"\n" +
				"        dout.write(dump());\n" +
				"        dout.flush();\n" +
				"        dout.close();\n" +
				"        } catch (FileNotFoundException e) {\n" +
				"        e.printStackTrace();\n" +
				"    } catch (IOException e) {\n" +
				"            e.printStackTrace();\n" +
				"        } catch (Exception e) {\n" +
				"            e.printStackTrace();\n" +
				"        " +

				"   } }\n" +
				"}";
	}

	/**
	 * Performed just before compiling blocks to allow for action when all blocks parsed
	 */
    @Override
    public void init() {
		// Move anything outside a method and within the class to a constructor block
        for (Block sub : getSubBlocks()) {
            moveToConstructor(sub);
        }

		Block block = getSuperBlock();
		// Get the package the class is within
		for(Block fileSub : block.getSubBlocks()){

			if(fileSub instanceof PackageBlock){
				packageBlock = (PackageBlock) fileSub;
			}
		}
    }

	// Moves all blocks that are inside the class and outside methods into the constructor block
	public void moveToConstructor(Block block){
        if (block instanceof MethodBlock || block instanceof ConstructorBlock) {
            return;
        }else {
            Block ref = block;
            constructorBlock.addBlock(ref);
            block.getSuperBlock().removeBlock(block);
            block.setSuperBlock(constructorBlock);

        }

        for (Block sub : block.getSubBlocks()) {
            moveToConstructor(sub);
        }
    }


	public String toString(){
		String paramString = "";
		for(Parameter parameter : parameters){
			paramString += parameter.getType() + ":" + parameter.getName() + "; ";
		}
		return name + " ( " + paramString + ")";
	}
}