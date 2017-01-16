package compiler.parser.structures;

import compiler.block.Block;
import compiler.block.structures.objects.ObjectMethodCallBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

/**
 * Parses calling of an objects method
 */
public class ObjectMethodCallParser extends Parser<ObjectMethodCallBlock> {

    @Override
    public boolean shouldParse(String line) {

        return line.matches("[a-zA-Z][a-zA-Z0-9]*\\.[a-zA-Z][a-zA-Z0-9]*\\(\\)");
    }



    @Override
    public ObjectMethodCallBlock parse(Block superBlock, Tokenizer tokenizer) {

        String variableName = tokenizer.nextToken().getToken(); // Get the string value of the next token.
        tokenizer.nextToken();
        String methodName = tokenizer.nextToken().getToken();

        return new ObjectMethodCallBlock(superBlock, variableName, methodName, null);
    }
}