package compiler.parser;

import compiler.block.Block;
import compiler.block.loops.WhileBlock;
import compiler.tokenizer.Token;
import compiler.tokenizer.Tokenizer;

/*
need to make parameter single variable names instead as cant define a variable in an ifs...
 */


public class WhileParser extends Parser<WhileBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("while[ ]*\\((.*)*\\):");
    }

    @Override
    public WhileBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  //skip while
        tokenizer.nextToken();  // skip (

        Token condition = tokenizer.nextToken();
        Token operator = tokenizer.nextToken();
        Token value = tokenizer.nextToken();

        return new WhileBlock(superBlock, condition.getToken() + operator.getToken() + value.getToken());
    }
}
