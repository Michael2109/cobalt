package compiler.parser.structure;

import compiler.block.Block;
import compiler.block.StaticMethodCallBlock;
import compiler.block.structure.ObjectMethodCallBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class ObjectMethodCallParser extends Parser<ObjectMethodCallBlock> {

    @Override
    public boolean shouldParse(String line) {

        return line.matches("[a-zA-Z][a-zA-Z0-9]*\\.[a-zA-Z][a-zA-Z0-9]*\\(\\)");
    }



    @Override
    public ObjectMethodCallBlock parse(Block superBlock, Tokenizer tokenizer) {

        String className = tokenizer.nextToken().getToken(); // Get the string value of the next token.
        tokenizer.nextToken();
        String methodName = tokenizer.nextToken().getToken();

        return new ObjectMethodCallBlock(superBlock, className, methodName, null);
    }
}