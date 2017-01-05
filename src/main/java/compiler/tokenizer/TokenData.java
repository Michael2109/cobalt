package compiler.tokenizer;

import java.util.regex.Pattern;

public class TokenData {

	private Pattern pattern;
	private TokenType type;
	
	public TokenData(Pattern pattern, TokenType type) {
		this.pattern = pattern;
		this.type = type;
	}
	
	public Pattern getPattern() {
		return pattern;
	}
	
	public TokenType getType() {
		return type;
	}
}