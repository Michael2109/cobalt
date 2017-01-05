package compiler.parser;

import compiler.block.Block;
import compiler.block.method.ConstructorBlock;
import compiler.tokenizer.Tokenizer;

public class ConstructorParser extends Parser<ConstructorBlock> {

	@Override
	public boolean shouldParse(String line) {



		return line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]*\\([a-zA-Z][a-zA-Z0-9]*[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*\\)[ ]*:");
	}



	@Override
	public ConstructorBlock parse(Block superBlock, Tokenizer tokenizer) {

		String type = "constructor";
		
		String name = tokenizer.nextToken().getToken(); // Get the string value of the next token.
		
		return new ConstructorBlock(superBlock, name, type, null);
	}
}