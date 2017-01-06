package compiler.block.loops;

import compiler.Parameter;
import compiler.block.Block;

public class WhileBlock extends Block {

    private String type = "while";
    String name;

    public WhileBlock(Block superBlock, String name) {
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

    public String getType() {
        return type;
    }

    @Override
    public String getOpeningCode() {
        return "\"while(" + name + "){\"+";
    }

    @Override
    public String getClosingCode() {
        return "\"}\"+";
    }

    @Override
    public String getBodyCode() {
        return "";
    }

    @Override
    public void run() {

    }
}