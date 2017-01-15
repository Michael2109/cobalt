package compiler.block.loops;

import compiler.Parameter;
import compiler.block.Block;

public class ForBlock extends Block {

    private String type = "for";
    private Parameter[] params;
    String name;

    public ForBlock(Block superBlock, String name) {
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
    public void init() {

    }

    @Override
    public void run() {

    }
}