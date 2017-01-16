package compiler.parser.imports;

import junit.framework.TestCase;
import org.junit.Test;

public class ImportParserTest extends TestCase {

    ImportParser importParser = new ImportParser();

    @Override
    public void setUp() throws Exception {
        super.setUp();
    }

    @Test
    public void testImportParser() {
        assertEquals(importParser.shouldParse("import file"), true);
        assertEquals(importParser.shouldParse("import directory.file"), true);
        assertEquals(importParser.shouldParse("import directory.directory.file"), true);
        assertEquals(importParser.shouldParse("import directory."), false);
    }
}
