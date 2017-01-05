package compiler.block.comments;

import compiler.block.Block;

public class CommentBlock extends Block {

    private String type = "comment";


    public CommentBlock(Block superBlock) {
        super(superBlock, false, false);

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

    @Override
    public void run() {

    }

    @Override
    public String getName() {
        return null;
    }

    @Override
    public String getValue() {
        return null;
    }
}