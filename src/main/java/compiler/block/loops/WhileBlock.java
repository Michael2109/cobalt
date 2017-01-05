package compiler.block.loops;

import compiler.Parameter;
import compiler.block.Block;

public class WhileBlock extends Block {

    private String type = "while";
    private Parameter[] params;
    public String variable, conditional, number, name = "";

    public WhileBlock(Block superBlock, String variable, String conditional, String number) {
        super(superBlock, true, false);
        this.variable = variable;
        this.conditional = conditional;
        this.number = number;
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