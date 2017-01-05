package compiler.parser;

import compiler.block.Block;
import compiler.block.MethodCallBlock;
import compiler.tokenizer.Tokenizer;

public class MethodCallParser extends Parser<MethodCallBlock> {

    @Override
    public boolean shouldParse(String line) {

        return line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*\\(\\)[ ]*;");
    }



    @Override
    public MethodCallBlock parse(Block superBlock, Tokenizer tokenizer) {

        String name = tokenizer.nextToken().getToken(); // Get the string value of the next token.

        return new MethodCallBlock(superBlock, name, "method_call", null);
    }
}