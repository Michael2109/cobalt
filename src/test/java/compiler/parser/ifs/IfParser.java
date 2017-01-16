package compiler.parser.ifs;

import compiler.block.Block;
import compiler.block.ifs.IfBlock;
import compiler.block.loops.WhileBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Token;
import compiler.tokenizer.TokenData;
import compiler.tokenizer.TokenType;
import compiler.tokenizer.Tokenizer;

public class IfParser extends Parser<IfBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("if[ ]*\\((.*)*\\)[ ]*:");
    }

    @Override
    public IfBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  //skip if
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

        return new IfBlock(superBlock, statement.trim().replaceAll(" +", " "));
    }
}
