package compiler.block.method;

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
		return "   {\n" +
				"            /* Build 'add' method */\n" +
				"            MethodVisitor mv = cw.visitMethod(\n" +
				"                    ACC_PUBLIC,                         // public method\n" +
				"                    \""+name+"\",                              // name\n" +
				"                    \"()V\",                            // descriptor\n" +
				"                    null,                               // signature (null means not generic)\n" +
				"                    null);                              // exceptions (array of strings)\n" +
				"\n";
	}

	@Override
	public String getBodyCode() {
		return "";
	}

	@Override
	public String getClosingCode() {
		return "       mv.visitInsn(RETURN);                      // Return integer from top of stack\n" +
				"            mv.visitMaxs(0, 0);                         // Specify max stack and local vars\n" +
				"        }";
	}


	public Parameter[] getParameters() {
		return params;
	}

	@Override
	public void run() {

	}

}