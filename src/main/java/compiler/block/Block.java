package compiler.block;

import java.util.ArrayList;
import java.util.Collections;

/**
 * Represents a block of code.
 */
public abstract class Block {

	protected Block superBlock;
	protected ArrayList<Block> subBlocks;

	// true ifs block can store sub blocks
	protected boolean container = false;

	private boolean variable = false;

	protected static int TOTAL_BLOCKS = 0;

	// Block ID used for reference when storing in local variable table
	protected int id;


	public Block(Block superBlock, boolean container, boolean variable) {
		this.superBlock = superBlock;
		this.subBlocks = new ArrayList<>();
		this.container = container;
		id = TOTAL_BLOCKS++;

	}
	
	public Block getSuperBlock() {
		return superBlock;
	}
	
	public ArrayList<Block> getBlockTree() {
		ArrayList<Block> blocks = new ArrayList<Block>();
		
		Block block = this;
		
		do {
			blocks.add(block);
			block = block.getSuperBlock();
		} while (block != null);
		
		Collections.reverse(blocks);
		
		return blocks;
	}
	
	public Block[] getSubBlocks() {
		return subBlocks.toArray(new Block[subBlocks.size()]);
	}
	
	public void addBlock(Block block) {
		subBlocks.add(block);
	}

	public abstract void run();

    public abstract String getName();
	public abstract String getValue();
	public abstract String getType();

	public abstract String getOpeningCode();
	public abstract String getBodyCode();
	public abstract String getClosingCode();

	@Override
	public String toString(){
		return getName() + " " + getValue() + " " + getType();
	}

	public void setId(int id) {
		this.id = id;
	}
	public int getId() {
		return id;
	}

	public boolean isContainer() {
		return container;
	}

	public void setContainer(boolean container) {
		this.container = container;
	}

	public boolean isVariable() {
		return variable;
	}

	public void setVariable(boolean variable) {
		this.variable = variable;
	}
}