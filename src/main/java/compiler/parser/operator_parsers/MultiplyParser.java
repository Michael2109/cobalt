package compiler.parser.operator_parsers;

import compiler.block.Block;
import compiler.block.operators.Multiply;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class MultiplyParser extends Parser<Multiply> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("[a-zA-Z][a-zA-Z0-9]* [*][=] [0-9]+");
    }

    @Override
    public Multiply parse(Block superBlock, Tokenizer tokenizer) {

        String name = tokenizer.nextToken().getToken();

        tokenizer.nextToken().getToken();

        String value = tokenizer.nextToken().getToken();

        return new Multiply(superBlock,name,value);
    }
}
