package compiler.block.ifs;

import compiler.Parameter;
import compiler.block.Block;

public class IfBlock extends Block {

    private String type = "ifs";
    private Parameter[] params;
    String name;

    public IfBlock(Block superBlock, String name) {
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
        return "\"if(true){\"+";
    }

    @Override
    public String getClosingCode() {
        return "\"}\"+";
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