package compiler.parser.imports;

import compiler.block.Block;
import compiler.block.imports.ImportBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

import java.io.File;

public class ImportParser extends Parser<ImportBlock> {

    @Override
    public boolean shouldParse(String line) {
        return line.matches("import [a-zA-Z][a-zA-Z0-9]*(\\.[a-zA-Z][a-zA-Z0-9]*)*");
    }



    @Override
    public ImportBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken(); // import



        String fileLoc = tokenizer.nextToken().getToken(); // Get the string value of the next token.;

        String nextToken = tokenizer.nextToken().getToken();
        String fileName = nextToken;
        while(!nextToken.equals("")){
            if(nextToken.equals(".")){
                fileLoc += "/";
            }else {
                fileLoc += nextToken;
            }
            fileName = nextToken;
            nextToken = tokenizer.nextToken().getToken();

        }

        int i = fileLoc.lastIndexOf("/");
        fileLoc =  (i > -1) ? fileLoc.substring(0, i) : fileLoc;

        return new ImportBlock(fileLoc, fileName);
    }
}