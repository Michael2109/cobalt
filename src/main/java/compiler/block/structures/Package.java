package compiler.block.structures;

import compiler.block.Block;

public class Package extends Block{

    public Package(Block superBlock, boolean container, boolean variable) {
        super(superBlock, container, variable);
    }

    @Override
    public void run() {

    }

    @Override
    public String getName() {
        return null;
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
        return null;
    }

    @Override
    public String getBodyCode() {
        return null;
    }

    @Override
    public String getClosingCode() {
        return null;
    }
}
