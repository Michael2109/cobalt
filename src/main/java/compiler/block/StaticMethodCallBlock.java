package compiler.block;

import compiler.Parameter;

public class StaticMethodCallBlock extends Block {

    private String name, type;
    private Parameter[] params;

    public StaticMethodCallBlock(Block superBlock, String name, String type, Parameter[] params) {
        super(superBlock, false, true);

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
        return "";
    }

    @Override
    public String getClosingCode() {
        return "";
    }

    @Override
    public String getBodyCode() {
        return name+"();";
    }

    public Parameter[] getParameters() {
        return params;
    }

    @Override
    public void run() {

    }

}