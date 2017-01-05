package compiler.parser;

import compiler.block.Block;
import compiler.block.ifs.IfBlock;
import compiler.tokenizer.Token;
import compiler.tokenizer.Tokenizer;

/*
need to make parameter single variable names instead as cant define a variable in an ifs...
 */


public class IfParser extends Parser<IfBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("if \\(([a-zA-Z][a-zA-Z0-9]*)*\\):");
    }

    @Override
    public IfBlock parse(Block superBlock, Tokenizer tokenizer) {

      tokenizer.nextToken();  //skip ifs
       tokenizer.nextToken();  // skip (
        Token first = tokenizer.nextToken();








        return new IfBlock(superBlock, first.getToken());
    }
}
