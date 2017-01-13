package compiler.parser.primitives;

import compiler.block.Block;
import compiler.block.primitives.FloatBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class FloatParser extends Parser<FloatBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("float [a-zA-Z][a-zA-Z0-9]* [=] [0-9]+[.][0-9]*f");
    }

    @Override
    public FloatBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  // skip float

        String name = tokenizer.nextToken().getToken();

        tokenizer.nextToken();

        String value = tokenizer.nextToken().getToken();
        tokenizer.nextToken();
        value += "." + tokenizer.nextToken().getToken();

        return new FloatBlock(superBlock,name,value);
    }
}
