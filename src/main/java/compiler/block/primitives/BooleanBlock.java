package compiler.block.primitives;

import compiler.block.Block;

public class BooleanBlock extends Block {

    private String type = "boolean";
    private String value;
    private String name;

    public BooleanBlock(Block superBlock, String name, String value) {
        super(superBlock, false, true);
        this.name = name;
        this.value = value;
    }

    @Override
    public void init() {

    }

    @Override
    public void run() {

    }

    public String getType() {
        return type;
    }

    @Override
    public String getOpeningCode() {
        return "";
    }


    @Override
    public String getBodyCode() {
        return "";
    }

    @Override
    public String getClosingCode() {
        return "";
    }


    public void setType(String type) {
        this.type = type;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
