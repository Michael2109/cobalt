package compiler.block.structures.methods;

import compiler.Parameter;
import compiler.block.Block;

public class ConstructorBlock extends Block {

	private String name, type;
	private Parameter[] params;

	public ConstructorBlock(Block superBlock, String name, String type, Parameter[] params) {
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
		return null;
	}

	@Override
	public String getClosingCode() {
		return null;
	}

	@Override
	public String getBodyCode() {
		return null;
	}

	public Parameter[] getParameters() {
		return params;
	}

	@Override
	public void run() {

	}

}