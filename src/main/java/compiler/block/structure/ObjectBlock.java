package compiler.block.structure;

import compiler.block.Block;
import compiler.tokenizer.Token;

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
        return null;
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
        return className.getToken() + " " + variableName.getToken() + " " + operator.getToken() + " " + newKeyword.getToken() + " " + initClassName.getToken() + "();";
    }

    @Override
    public String getClosingCode() {
        return "";
    }
}
