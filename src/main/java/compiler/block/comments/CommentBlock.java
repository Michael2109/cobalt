package compiler.block.comments;

import compiler.block.Block;

public class CommentBlock extends Block {

    private String type = "comment";


    public CommentBlock(Block superBlock) {
        super(superBlock, false, false);

    }

    @Override
    public void init() {

    }

    @Override
    public String getName() {
        return null;
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
    public String getBodyCode() {
        return null;
    }

    @Override
    public String getClosingCode() {
        return null;
    }

    @Override
    public String toString() {
        return "Comment";
    }
}