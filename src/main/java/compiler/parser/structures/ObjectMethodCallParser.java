package compiler.parser.structures;

import compiler.Parameter;
import compiler.Utils;
import compiler.block.Block;
import compiler.block.structures.objects.ObjectMethodCallBlock;
import compiler.parser.Parser;
import compiler.symbol_table.SymbolTable;
import compiler.tokenizer.Tokenizer;

import java.util.ArrayList;
import java.util.List;

/**
 * Parses calling of an objects method
 */
public class ObjectMethodCallParser extends Parser<ObjectMethodCallBlock> {

    @Override
    public boolean shouldParse(String line) {

        return line.matches("[a-zA-Z][a-zA-Z0-9]*\\.[a-zA-Z][a-zA-Z0-9]*\\((.*)*\\)[ ]*");
    }



    @Override
    public ObjectMethodCallBlock parse(Block superBlock, Tokenizer tokenizer) {

        String variableName = tokenizer.nextToken().getToken(); // Get the string value of the next token.
        tokenizer.nextToken();
        String methodName = tokenizer.nextToken().getToken();

        tokenizer.nextToken(); // ")"

        String nextToken = tokenizer.nextToken().getToken();

        String paramType = "";
        String paramName = "";
        List<Parameter> parameters = new ArrayList<>();
        while (!nextToken.equals(")")) {
            if (nextToken.equals(",")) {
                nextToken = tokenizer.nextToken().getToken();
                continue;
            }

            // todo find the paramType. Utilities add a method to get the type
            paramName = nextToken.trim();
            parameters.add(new Parameter(paramType, paramName));

            nextToken = tokenizer.nextToken().getToken();
        }

        return new ObjectMethodCallBlock(superBlock, variableName, methodName, parameters.toArray(new Parameter[parameters.size()]));
    }
}