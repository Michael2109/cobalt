package compiler.block.packages;

public class PackageBlock extends Block {

    public String directory;

    public PackageBlock(String directory) {
        super(null, false, false);
        this.directory = directory;
    }

    @Override
    public void init() {

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
    public String getBodyCode() {
        return null;
    }

    @Override
    public String getClosingCode() {
        return null;
    }

}