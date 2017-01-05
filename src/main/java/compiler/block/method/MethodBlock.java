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
		return "cc.addMethod(CtNewMethod.make(\"public static void "+name+"(){ \"+" ;
	}

	@Override
	public String getClosingCode() {
		return "\"}\", cc));";
	}

	@Override
	public String getBodyCode() {
		return "";
	}

	public Parameter[] getParameters() {
		return params;
	}

	@Override
	public void run() {

	}

}