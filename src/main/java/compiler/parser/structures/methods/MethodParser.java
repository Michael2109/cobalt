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
		return line.matches("[a-zA-Z][a-zA-Z0-9]*[ ]+[a-zA-Z][a-zA-Z0-9]*[ ]*\\((.*)*\\)[ ]*:");
	}

	@Override
	public MethodBlock parse(Block superBlock, Tokenizer tokenizer) {

		String type = tokenizer.nextToken().getToken(); // method return type

		String name = tokenizer.nextToken().getToken(); // method name

		tokenizer.nextToken(); // "("

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
		
		return new MethodBlock(superBlock, name, type, parameters.toArray(new Parameter[parameters.size()]));
	}
}