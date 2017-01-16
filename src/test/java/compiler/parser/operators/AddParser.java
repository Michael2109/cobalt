package compiler.parser.operators;

import compiler.block.Block;
import compiler.block.operators.AddBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class AddParser extends Parser<AddBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*[+][=][ ]*[0-9]+");
    }

    @Override
    public AddBlock parse(Block superBlock, Tokenizer tokenizer) {

        String name = tokenizer.nextToken().getToken();

        tokenizer.nextToken().getToken();

        String value = tokenizer.nextToken().getToken();


        return new AddBlock(superBlock, name, value);
    }
}
