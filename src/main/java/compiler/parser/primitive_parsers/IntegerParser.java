package compiler.parser.primitive_parsers;

import compiler.block.Block;
import compiler.block.primitives.IntegerBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class IntegerParser extends Parser<IntegerBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("int [a-zA-Z][a-zA-Z0-9]* [=] [0-9]+");
    }

    @Override
    public IntegerBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  // skip int

        String name = tokenizer.nextToken().getToken();

        tokenizer.nextToken();

        String value = tokenizer.nextToken().getToken();

        return new IntegerBlock(superBlock,name,value);
    }
}
