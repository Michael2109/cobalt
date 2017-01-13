package compiler.block.structures;

import compiler.block.Block;

/**
 * Represents a class.
 */
public class ClassBlock extends Block {
	
	private String name;

	public ClassBlock(Block superBlock, String name) {
		super(superBlock, true, false);
		this.name = name;

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
		return   "package asm;\n" +
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
				"                    \"asm/"+name+"\",    // package and name\n" +
				"                    null,                               // signature (null means not generic)\n" +
				"                    \"java/lang/Object\",                 // superclass\n" +
				"                    new String[]{}); // interfaces\n" +
				"        }" +
				"" +
				"\n\n\n// Build the constructor\n" +
				"        {\n" +
				"            MethodVisitor con = cw.visitMethod(\n" +
				"                    ACC_PUBLIC,                         // public method\n" +
				"                    \"<init>\",                           // method name\n" +
				"                    \"()V\",                              // descriptor\n" +
				"                    null,                               // signature (null means not generic)\n" +
				"                    null);                              // exceptions (array of strings)\n" +
				"\n" +
				"            con.visitCode();                            // Start the code for this method\n" +
				"            con.visitVarInsn(ALOAD, 0);                 // Load \"this\" onto the stack\n" +
				"\n" +
				"            con.visitMethodInsn(INVOKESPECIAL,          // Invoke an instance method (non-virtual)\n" +
				"                    \"java/lang/Object\",                 // Class on which the method is defined\n" +
				"                    \"<init>\",                           // Name of the method\n" +
				"                    \"()V\",                              // Descriptor\n" +
				"                    false);                             // Is this class an interface?\n" +
				"\n" +
				"            con.visitInsn(RETURN);                      // End the constructor method\n" +
				"            con.visitMaxs(1, 1);                        // Specify max stack and local vars\n" +
				"        }";

	}

	@Override
	public String getBodyCode() {
		return "";
	}

	@Override
	public String getClosingCode() {
		return "return cw.toByteArray();}\n" +
				"    public static void main(String [] args){\n   " +
				"  DataOutputStream dout = null;\n" +
				"        try {\n" +
				"            dout = new DataOutputStream(new FileOutputStream(\"build/classes/main/asm/GeneratedAsmCode.class\"));\n" +
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

	@Override
	public void run() {

	}
}