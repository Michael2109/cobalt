package compiler.parser.structure;

import compiler.block.Block;
import compiler.block.StaticMethodCallBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class StaticMethodCallParser extends Parser<StaticMethodCallBlock> {

    @Override
    public boolean shouldParse(String line) {

        return line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*\\(\\)[ ]*");
    }



    @Override
    public StaticMethodCallBlock parse(Block superBlock, Tokenizer tokenizer) {

        String name = tokenizer.nextToken().getToken(); // Get the string value of the next token.

        return new StaticMethodCallBlock(superBlock, name, "method_call", null);
    }
}