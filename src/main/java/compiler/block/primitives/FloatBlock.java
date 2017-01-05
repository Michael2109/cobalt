package compiler.block.primitives;

import compiler.block.Block;

public class FloatBlock extends Block {

    private String type = "float";
    private String value;
    private String name;

    public FloatBlock(Block superBlock, String name, String value) {
        super(superBlock, false, true);
        this.name = name;
        this.value = value;
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
    public String getClosingCode() {
        return "";
    }

    @Override
    public String getBodyCode() {
        return "\"float "+name+" = "+value+";\"+";
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
