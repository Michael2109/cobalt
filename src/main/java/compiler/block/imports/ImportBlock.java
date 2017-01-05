package compiler.block.imports;

import compiler.Parameter;
import compiler.block.Block;

public class ImportBlock extends Block {

    private String name, type;
    private Parameter[] params;

    public ImportBlock(String name, String type, Parameter[] params) {
        super(null, false, false);

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