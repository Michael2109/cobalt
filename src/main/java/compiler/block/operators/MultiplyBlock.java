package compiler.block.operators;

import compiler.block.Block;

public class MultiplyBlock extends Block {

    private String type = "multiply";
    private String value;
    private String name;

    public MultiplyBlock(Block superBlock, String name, String value) {
        super(superBlock, false, false);
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
        return null;
    }

    @Override
    public String getBodyCode() {
        return "mv.visitLdcInsn("+value+");\n"+
                "mv.visitVarInsn(ILOAD,"+getId()+");\n" +
                "mv.visitInsn(IMUL);\n" +
                "mv.visitVarInsn(ISTORE,"+getId()+");\n";
    }

    @Override
    public String getClosingCode() {
        return null;
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
