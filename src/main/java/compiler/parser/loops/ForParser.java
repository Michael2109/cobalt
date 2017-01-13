package compiler.parser.loops;

import compiler.block.Block;
import compiler.block.loops.ForBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Token;
import compiler.tokenizer.Tokenizer;

/*
need to make parameter single variable names instead as cant define a variable in an ifs...
 */


public class ForParser extends Parser<ForBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("for \\([a-zA-Z][a-zA-Z0-9]* [a-zA-Z][a-zA-Z0-9]* = [0-9]+([.][0-9]*)?; [a-zA-Z][a-zA-Z0-9]* [<|>|<=|>=|==] [0-9]+([.][0-9]*)?; [a-zA-Z][a-zA-Z0-9]*\\+\\+\\):");
    }

    @Override
    public ForBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  //skip ifs
        tokenizer.nextToken();  // skip (
        Token first = tokenizer.nextToken();








        return new ForBlock(superBlock, first.getToken());
    }
}
