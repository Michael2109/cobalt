package compiler.parser.structures;

import compiler.block.Block;
import compiler.block.structures.objects.ObjectBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Token;
import compiler.tokenizer.Tokenizer;

/**
 * Creation of a new instance of a class
 */
public class ObjectParser extends Parser<ObjectBlock> {
    @Override
    public boolean shouldParse(String line) {
        return line.matches("[a-zA-Z][a-zA-Z0-9]* [a-zA-Z][a-zA-Z0-9]* [=][ ]*new[ ]*[a-zA-Z][a-zA-Z0-9]*\\(\\)");
    }

    @Override
    public ObjectBlock parse(Block superBlock, Tokenizer tokenizer) {

        String className = tokenizer.nextToken().getToken();
        String variableName = tokenizer.nextToken().getToken();
        String operator = tokenizer.nextToken().getToken();
        String newKeyword = tokenizer.nextToken().getToken();
        String initClassName = tokenizer.nextToken().getToken();

        return new ObjectBlock(superBlock, className, variableName, operator, newKeyword, initClassName);
    }
}
