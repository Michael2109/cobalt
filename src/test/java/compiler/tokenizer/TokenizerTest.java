package compiler.tokenizer;

import org.junit.Test;

public class TokenizerTest {

    @Test
    public void testTokenizer(){
        String code =
                "MyCode myCode = new MyCode()"
                ;

        Tokenizer tokenizer = new Tokenizer(code);

        while (tokenizer.hasNextToken()) {
            Token t = tokenizer.nextToken();
        }
    }

}
