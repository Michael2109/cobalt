package compiler.parser.operators;

import compiler.block.Block;
import compiler.block.operators.DivideBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class DivideParser extends Parser<DivideBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("[a-zA-Z][a-zA-Z0-9]* [/][=] [0-9]+");
    }

    @Override
    public DivideBlock parse(Block superBlock, Tokenizer tokenizer) {

        String name = tokenizer.nextToken().getToken();

        tokenizer.nextToken().getToken();

        String value = tokenizer.nextToken().getToken();

        return new DivideBlock(superBlock,name,value);
    }
}
