package compiler.parser.ifs;

import compiler.block.Block;
import compiler.block.ifs.IfBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Token;
import compiler.tokenizer.TokenData;
import compiler.tokenizer.TokenType;
import compiler.tokenizer.Tokenizer;

/*
need to make parameter single variable names instead as cant define a variable in an ifs...
 */


public class IfParser extends Parser<IfBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("if \\((.*)*\\):");
    }

    @Override
    public IfBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  //skip if
        tokenizer.nextToken();  // skip (
        Token first = tokenizer.nextToken();
        Token operator = tokenizer.nextToken();

        if (operator.getToken().equals(")")) {
            return new IfBlock(superBlock, first.getToken());
        } else {
            Token value = tokenizer.nextToken();
            return new IfBlock(superBlock, first.getToken() + " " + operator.getToken() + " " + value.getToken());
        }
    }
}
