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
        return line.matches("while[ ]*\\([a-zA-Z][a-zA-Z0-9]*[ ]*[<|>|<=|>=|==][ ]*[0-9]+\\):");
    }

    @Override
    public WhileBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  //skip while
        tokenizer.nextToken();  // skip (
        Token variable = tokenizer.nextToken();

        Token operator = tokenizer.nextToken();

        Token number = tokenizer.nextToken();









        return new WhileBlock(superBlock, variable.getToken(), operator.getToken(), number.getToken());
    }
}
