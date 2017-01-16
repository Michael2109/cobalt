package compiler.parser.structures.classes;

import compiler.Parameter;
import compiler.block.Block;
import compiler.block.structures.classes.ClassBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

import java.util.ArrayList;
import java.util.List;

public class ClassParser extends Parser<ClassBlock> {

    @Override
    public boolean shouldParse(String line) {
        return line.matches("class[ ]+[a-zA-Z][a-zA-Z0-9]*\\((.*)*\\):");
    }


    @Override
    public ClassBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();
        String className = tokenizer.nextToken().getToken();
        tokenizer.nextToken(); // (


        String nextToken = tokenizer.nextToken().getToken();
        int i = 0;
        String paramType = "";
        String paramName = "";
        List<Parameter> parameters = new ArrayList<>();
        while (!nextToken.equals(")")) {
            //	parameters += " " + nextToken + " ";
            if (nextToken.equals(",")) {
                nextToken = tokenizer.nextToken().getToken();
                continue;
            }

            if (i % 2 == 0) {
                paramType = nextToken.trim();
            } else {
                paramName = nextToken.trim();
                parameters.add(new Parameter(paramType, paramName));
            }
            nextToken = tokenizer.nextToken().getToken();
            i++;
        }

        return new ClassBlock(superBlock, className, parameters);
    }
}