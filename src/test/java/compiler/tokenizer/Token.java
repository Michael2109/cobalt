package compiler.tokenizer;

public class Token {

    private String token;
    private TokenType type;

    public Token(String token, TokenType type) {
        this.token = token;
        this.type = type;
    }

    public String getToken() {
        return token;
    }

    public TokenType getType() {
        return type;
    }

    @Override
    public String toString() {
        return token;
    }
}