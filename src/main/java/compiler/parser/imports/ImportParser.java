package compiler.parser.imports;

import compiler.block.Block;
import compiler.block.imports.ImportBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class ImportParser extends Parser<ImportBlock> {

    @Override
    public boolean shouldParse(String line) {

        return line.matches("import [a-zA-Z][a-zA-Z0-9]*[ ]*");
    }



    @Override
    public ImportBlock parse(Block superBlock, Tokenizer tokenizer) {

        tokenizer.nextToken();

        String name = tokenizer.nextToken().getToken(); // Get the string value of the next token.

        return new ImportBlock(name, null, null);
    }
}