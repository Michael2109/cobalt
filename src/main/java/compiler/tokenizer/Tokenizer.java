package compiler.tokenizer;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Tokenizer {

	private ArrayList<TokenData> tokenDatas;
	
	private String str;
	
	private Token lastToken;
	private boolean pushBack;
	
	public Tokenizer(String str) {
		this.tokenDatas = new ArrayList<TokenData>();
		this.str = str;
		

		tokenDatas.add(new TokenData(Pattern.compile("^((-)?[0-9]+)"), TokenType.INTEGER_LITERAL));
		tokenDatas.add(new TokenData(Pattern.compile("^((-)?[0-9]+[.][0-9])"), TokenType.DOUBLE_LITERAL));
		tokenDatas.add(new TokenData(Pattern.compile("^([+][=])"), TokenType.ADD_OPERATOR));
		tokenDatas.add(new TokenData(Pattern.compile("^([-][=])"), TokenType.SUBTRACT_OPERATOR));
		tokenDatas.add(new TokenData(Pattern.compile("^([*][=])"), TokenType.MULTIPLY_OPERATOR));
		tokenDatas.add(new TokenData(Pattern.compile("^([/][=])"), TokenType.DIVIDE_OPERATOR));
		tokenDatas.add(new TokenData(Pattern.compile("^(\".*\")"), TokenType.STRING_LITERAL));
		tokenDatas.add(new TokenData(Pattern.compile("^([;])"), TokenType.END_STATEMENT));
		tokenDatas.add(new TokenData(Pattern.compile("^([:])"), TokenType.COLON));
		tokenDatas.add(new TokenData(Pattern.compile("^([==])"), TokenType.EQUAL_TO));
		tokenDatas.add(new TokenData(Pattern.compile("^([<])"), TokenType.SMALLER_THAN));
		tokenDatas.add(new TokenData(Pattern.compile("^([<=])"), TokenType.SMALLER_THAN_EQUAL));
		tokenDatas.add(new TokenData(Pattern.compile("^([>])"), TokenType.LARGER_THAN));
		tokenDatas.add(new TokenData(Pattern.compile("^([>=])"), TokenType.LARGER_THAN_EQUAL));
		tokenDatas.add(new TokenData(Pattern.compile("^([a-zA-Z][a-zA-Z0-9]*)"), TokenType.IDENTIFIER));


		for (String t : new String[] { "=", "\\(", "\\)", "\\.", "\\,", "\\'" }) {
			tokenDatas.add(new TokenData(Pattern.compile("^(" + t + ")"), TokenType.TOKEN));
		}
	}
	
	public Token nextToken() {
		str = str.trim();
		
		if (pushBack) {
			pushBack = false;
			return lastToken;
		}
		
		if (str.isEmpty()) {
			return (lastToken = new Token("", TokenType.EMPTY));
		}
		
		for (TokenData data : tokenDatas) {
			Matcher matcher = data.getPattern().matcher(str);
			
			if (matcher.find()) {
				String token = matcher.group().trim();
				str = matcher.replaceFirst("");
				
				if (data.getType() == TokenType.STRING_LITERAL) {
					return (lastToken = new Token(token.substring(1, token.length() - 1), TokenType.STRING_LITERAL));
				}
				
				else {
					return (lastToken = new Token(token, data.getType()));
				}
			}
		}
		
		throw new IllegalStateException("Could not parse " + str);
	}
	
	public boolean hasNextToken() {
		return !str.isEmpty();
	}
	
	public void pushBack() {
		if (lastToken != null) {
			this.pushBack = true;
		}
	}
}