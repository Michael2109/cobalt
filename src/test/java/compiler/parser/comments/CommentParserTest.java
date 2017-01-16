package compiler.parser.comments;

import junit.framework.TestCase;
import org.junit.Test;

public class CommentParserTest extends TestCase {

    CommentParser commentParser;

    @Override
    public void setUp() throws Exception {
        super.setUp();
        commentParser = new CommentParser();
    }

    @Test
    public void testCommentParsing() {
        assertEquals(commentParser.shouldParse("// Comment"), true);
        assertEquals(commentParser.shouldParse("Not a comment"), false);
    }
}
