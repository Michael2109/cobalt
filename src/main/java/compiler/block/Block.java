package compiler.block;

import java.util.ArrayList;
import java.util.Collections;

/**
 * Represents a block of code.
 */
public abstract class Block {

    protected static int TOTAL_BLOCKS = 0;
    protected ArrayList<Block> subBlocks;

    // true ifs block can store sub blocks
    protected boolean container = false;
    // Block ID used for reference when storing in local variable table
    protected int id;
    private Block superBlock;
    private boolean variable = false;


    public Block(Block superBlock, boolean container, boolean variable) {
        this.setSuperBlock(superBlock);
        this.subBlocks = new ArrayList<>();
        this.container = container;
        id = TOTAL_BLOCKS++;

    }

    public Block getSuperBlock() {
        return superBlock;
    }

    public void setSuperBlock(Block superBlock) {
        this.superBlock = superBlock;
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

    // Removes a block
    public void removeBlock(Block block) {
        subBlocks.remove(block);
    }

    // Called before looping through blocks to generate code. Allows for method to be called when all blocks are loaded
    public abstract void init();

    public abstract String getName();

    public abstract String getValue();

    public abstract String getType();

    public abstract String getOpeningCode();

    public abstract String getBodyCode();

    public abstract String getClosingCode();

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
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