package compiler.parser.ifs;

import junit.framework.TestCase;
import org.junit.Test;

public class IfParserTest extends TestCase {

    IfParser ifParser;

    @Override
    public void setUp() throws Exception {
        super.setUp();
        ifParser = new IfParser();
    }

    @Test
    public void testCommentParser() {
        assertEquals(ifParser.shouldParse("if(x < 10):"), true);
        assertEquals(ifParser.shouldParse("if(x):"), true);
        assertEquals(ifParser.shouldParse("if (x) :"), true);
        assertEquals(ifParser.shouldParse("Not an if"), false);
    }

}
