package compiler.block.structures;

import compiler.block.Block;

/**
 * Represents the whole file.
 */
public class FileBlock extends Block {

    private String name;

    public FileBlock() {
        super(null, true, false);
    }

    public String getName() {
        return name;
    }

    @Override
    public String getValue() {
        return null;
    }

    @Override
    public String getType() {
        return null;
    }

    @Override
    public String getOpeningCode() {
        return "";
    }

    @Override
    public String getClosingCode() {
        return "";
    }

    @Override
    public String getBodyCode() {
        return "";
    }


    @Override
    public void init() {

    }

}