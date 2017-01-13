package compiler.block.structures;

import compiler.Parameter;
import compiler.block.Block;

public class ObjectMethodCallBlock extends Block {

    private String className, methodName, type;
    private Parameter[] params;

    public ObjectMethodCallBlock(Block superBlock, String className, String methodName,Parameter[] params) {
        super(superBlock, false, true);

        this.className = className;
        this.methodName = methodName;

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
        return className + "." + methodName+"();";
    }

    public Parameter[] getParameters() {
        return params;
    }

    @Override
    public void run() {

    }

    @Override
    public String getName() {
        return null;
    }

}