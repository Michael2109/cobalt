package compiler.parser.structures.methods;

import compiler.Parameter;
import compiler.block.Block;
import compiler.block.structures.methods.MethodBlock;
import compiler.parser.Parser;
import compiler.tokenizer.Tokenizer;

import java.util.ArrayList;
import java.util.List;

public class MethodParser extends Parser<MethodBlock> {

	@Override
	public boolean shouldParse(String line) {
		return line.matches("void [a-zA-Z][a-zA-Z0-9]*[ ]*\\([a-zA-Z][a-zA-Z0-9]*[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*\\)[ ]*:");
	}

	@Override
	public MethodBlock parse(Block superBlock, Tokenizer tokenizer) {

		String type = tokenizer.nextToken().getToken();
		
		String name = tokenizer.nextToken().getToken(); // Get the string value of the next token.

		// Get the parameters
		List<Parameter> parameters = new ArrayList<>();
		
		return new MethodBlock(superBlock, name, type, parameters.toArray(new Parameter[parameters.size()]));
	}
}