package compiler.parser.primitives;

import compiler.block.Block;
import compiler.block.primitives.DoubleBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class DoubleParser extends Parser<DoubleBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("double [a-zA-Z][a-zA-Z0-9]* [=] [0-9]+[.][0-9]*");
    }

    @Override
    public DoubleBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  // skip int

        String name = tokenizer.nextToken().getToken();

        tokenizer.nextToken();

        String value = tokenizer.nextToken().getToken();
        tokenizer.nextToken();
        value += "." + tokenizer.nextToken().getToken();

        return new DoubleBlock(superBlock,name,value);
    }
}
