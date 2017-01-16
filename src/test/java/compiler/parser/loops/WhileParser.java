package compiler.parser.loops;

import compiler.block.Block;
import compiler.block.loops.WhileBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Token;
import compiler.tokenizer.Tokenizer;

/*
need to make parameter single variable names instead as cant define a variable in an ifs...
 */


public class WhileParser extends Parser<WhileBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("while[ ]+\\((.*)*\\):");
    }

    @Override
    public WhileBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  //skip while
        tokenizer.nextToken();  // skip (

        String statement = "";
        String nextToken = tokenizer.nextToken().getToken();
        while (!nextToken.equals(")")) {

            if (nextToken.equals("="))
                statement += nextToken;
            else
                statement += " " + nextToken + " ";
            nextToken = tokenizer.nextToken().getToken();
        }

        return new WhileBlock(superBlock, statement.trim().replaceAll(" +", " "));
    }
}
