package compiler.block.structures.methods;

import compiler.Parameter;
import compiler.block.Block;

public class MethodBlock extends Block {
	
	private String name, type;
	private Parameter[] params;

	public MethodBlock(Block superBlock, String name, String type, Parameter[] params) {
		super(superBlock, true, false);
		
		this.name = name;
		this.type = type;
		this.params = params;

		for (Parameter param : params) {
			System.out.println(param);
		}
	}

	public Parameter[] getParameters() {
		return params;
	}

	@Override
	public void init() {

	}

	public String getName() {
		return name;
	}

	@Override
	public String getValue() {
		return null;
	}

	public String getType() {
		return type;
	}

	@Override
	public String getOpeningCode() {
		System.out.println(name);
		if(!name.equals("main")) {
			return "   {\n" +
					"            /* Build '"+name+"' method */\n" +
					"            MethodVisitor mv = cw.visitMethod(\n" +
					"                    ACC_PUBLIC,                         // public method\n" +
					"                    \"" + name + "\",                              // name\n" +
					"                    \"()V\",                            // descriptor\n" +
					"                    null,                               // signature (null means not generic)\n" +
					"                    null);                              // exceptions (array of strings)\n" +
					"\n";
		} else{
			return "{\n" +
					"// Main Method\n" +
					"MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, \"main\", \"([Ljava/lang/String;)V\", null, null);\n" +
					"mv.visitCode();\n" +
					"Label mainL0 = new Label();\n" +
					"mv.visitLabel(mainL0);\n" +
					"Label mainL1 = new Label();\n" +
					"mv.visitLabel(mainL1);\n" +
					"mv.visitLocalVariable(\"args\", \"[Ljava/lang/String;\", null, mainL0, mainL1, 0);";
		}
	}

	@Override
	public String getBodyCode() {
		return "";
	}

	@Override
	public String getClosingCode() {
		return "mv.visitInsn(RETURN);                      // Return integer from top of stack\n" +
				"mv.visitMaxs(0, 0);\n" +
				"mv.visitEnd();\n" +
				"}\n";
	}

}