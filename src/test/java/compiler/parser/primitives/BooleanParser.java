package compiler.parser.primitives;

import compiler.block.Block;
import compiler.block.primitives.BooleanBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class BooleanParser extends Parser<BooleanBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("boolean[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*[=][ ]*(true|false)");
    }

    @Override
    public BooleanBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  // skip boolean

        String name = tokenizer.nextToken().getToken();

        tokenizer.nextToken(); // =

        String value = tokenizer.nextToken().getToken();

        return new BooleanBlock(superBlock, name, value);
    }
}
