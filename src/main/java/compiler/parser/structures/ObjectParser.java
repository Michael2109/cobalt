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

        Token className = tokenizer.nextToken();
        Token variableName = tokenizer.nextToken();
        Token operator = tokenizer.nextToken();
        Token newKeyword = tokenizer.nextToken();
        Token initClassName = tokenizer.nextToken();

        System.out.println(className + " " + variableName + " " + operator + " " + newKeyword + " " + initClassName);

        return new ObjectBlock(superBlock, className, variableName , operator, newKeyword , initClassName);
    }
}
