package compiler.block.structures.objects;

import compiler.block.Block;
import compiler.tokenizer.Token;

// Creation of a new object and storing to a variable
public class ObjectBlock extends Block {

    Token className;
    Token variableName;
    Token operator;
    Token newKeyword;
    Token initClassName;

    public ObjectBlock(Block superBlock, Token className, Token variableName , Token operator, Token newKeyword , Token initClassName) {
        super(superBlock, false, true);
        this.className = className;
        this.variableName = variableName;
        this.operator = operator;
        this.newKeyword = newKeyword;
        this.initClassName = initClassName;
    }

    @Override
    public void run() {

    }

    @Override
    public String getName() {
        return variableName.getToken();
    }

    @Override
    public String getValue() {
        return null;
    }

    @Override
    public String getType() {
        return null;
    }

    @Override
    public String getOpeningCode() {
        return "";
    }

    @Override
    public String getBodyCode() {
        return "mv.visitTypeInsn(NEW, \"asm/"+className+"\");\n" +
                "mv.visitInsn(DUP);\n" +
                "mv.visitMethodInsn(INVOKESPECIAL, \"asm/"+className+"\", \"<init>\", \"()V\", false);\n" +
                "mv.visitVarInsn(ASTORE,"+ getId() +");\n";
    }

    @Override
    public String getClosingCode() {
        return "";
    }
}
