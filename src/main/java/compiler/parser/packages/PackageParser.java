package compiler.parser.packages;

import compiler.block.Block;
import compiler.block.imports.ImportBlock;
import compiler.block.packages.PackageBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class PackageParser extends Parser<PackageBlock> {

    @Override
    public boolean shouldParse(String line) {
        return line.matches("package [a-zA-Z][a-zA-Z0-9]*(\\.[a-zA-Z][a-zA-Z0-9]*)*");
    }



    @Override
    public PackageBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken(); // import
        String directory = tokenizer.nextToken().getToken(); // Get the string value of the next token.;
        String nextToken = tokenizer.nextToken().getToken();

        while(!nextToken.equals("")){
            if(nextToken.equals(".")){
                directory += "/";
            }else {
                directory += nextToken;
            }
            nextToken = tokenizer.nextToken().getToken();
        }

        return new PackageBlock(directory);
    }
}