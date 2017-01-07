package compiler.block.structure;

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
		return   "public class " + name + " {";

	}

	@Override
	public String getClosingCode() {
		return "}";
	}

	@Override
	public String getBodyCode() {
		return "";
	}


	@Override
	public void run() {

	}
}