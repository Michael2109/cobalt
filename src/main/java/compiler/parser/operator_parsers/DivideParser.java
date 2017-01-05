package compiler.parser.operator_parsers;

import compiler.block.Block;
import compiler.block.operators.Divide;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class DivideParser extends Parser<Divide> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("[a-zA-Z][a-zA-Z0-9]* [/][=] [0-9]+;");
    }

    @Override
    public Divide parse(Block superBlock, Tokenizer tokenizer) {

        String name = tokenizer.nextToken().getToken();

        tokenizer.nextToken().getToken();

        String value = tokenizer.nextToken().getToken();

        return new Divide(superBlock,name,value);
    }
}
