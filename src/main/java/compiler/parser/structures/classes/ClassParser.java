package compiler.parser.structures.classes;

import compiler.block.Block;
import compiler.block.structures.classes.ClassBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

public class ClassParser extends Parser<ClassBlock> {

	@Override
	public boolean shouldParse(String line) {
		return line.matches("class[ ]+[a-zA-Z][a-zA-Z0-9]*\\(\\):");
	}



	@Override
	public ClassBlock parse(Block superBlock, Tokenizer tokenizer) {

		tokenizer.nextToken();
		String className = tokenizer.nextToken().getToken();

		return new ClassBlock(superBlock, className);
	}
}