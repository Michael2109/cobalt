package compiler.parser;

import compiler.block.Block;
import compiler.block.PrintBlock;
import compiler.tokenizer.Tokenizer;

public class PrintParser extends Parser<PrintBlock> {

    public boolean printVariable = false;

    @Override
    public boolean shouldParse(String line) {

        if(line.matches("print[ ]*\\([\"].*[\"]\\);")){
            printVariable = false;
            return true;
        }else if(line.matches("print[ ]*\\([\"]?.*[\"]?\\);")){
            printVariable = true;
            return true;
        }

        return false;
    }

    @Override
    public PrintBlock parse(Block superBlock, Tokenizer tokenizer) {
        tokenizer.nextToken(); // skip print

        tokenizer.nextToken(); // skip (
        String value = tokenizer.nextToken().getToken();

        return new PrintBlock(superBlock,value, printVariable);
    }
}
