package compiler.parser.primitive_parsers;

import compiler.block.Block;
import compiler.block.primitives.CharacterBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class CharacterParser extends Parser<CharacterBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("char [a-zA-Z][a-zA-Z0-9]* [=] '[a-zA-Z0-9]';");
    }

    @Override
    public CharacterBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();  // skip char

        String name = tokenizer.nextToken().getToken();  //get name

        tokenizer.nextToken();  // skip =

        //System.out.println(tokenizer.nextToken().getToken());  // skip '
        tokenizer.nextToken();
        String value = tokenizer.nextToken().getToken();

        return new CharacterBlock(superBlock,name,value);
    }
}
