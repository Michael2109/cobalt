package compiler.block.imports;

import compiler.block.Block;

public class ImportBlock extends Block {

    public String directory, fileName;

    public ImportBlock(String directory, String fileName) {
        super(null, false, false);
        this.directory = directory;
        this.fileName = fileName;
    }

    public String getName() {
        return null;
    }

    @Override
    public String getValue() {
        return null;
    }

    public String getType() {
        return null;
    }

    @Override
    public String getOpeningCode() {
        return null;
    }

    @Override
    public String getClosingCode() {
        return null;
    }

    @Override
    public String getBodyCode() {
        return null;
    }


    @Override
    public void init() {

    }

}