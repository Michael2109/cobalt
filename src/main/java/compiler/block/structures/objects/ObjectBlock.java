package compiler.block.structures.objects;

import compiler.block.Block;
import compiler.block.imports.ImportBlock;
import compiler.block.structures.FileBlock;
import compiler.tokenizer.Token;

// Creation of a new object and storing to a variable
public class ObjectBlock extends Block {

    Token className;
    Token variableName;
    Token operator;
    Token newKeyword;
    Token initClassName;

    String directory = "";

    public ObjectBlock(Block superBlock, Token className, Token variableName , Token operator, Token newKeyword , Token initClassName) {
        super(superBlock, false, true);
        this.className = className;
        this.variableName = variableName;
        this.operator = operator;
        this.newKeyword = newKeyword;
        this.initClassName = initClassName;
    }

    @Override
    public void init() {
        directory = getDirectory();
    }

    // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
    public String getDirectory(){
        // Get the FileBlock to find the imports
        Block block = this;
        while(!(block instanceof FileBlock)){
            block = block.getSuperBlock();
        }

        // Get the directory of the Object
        for(Block sub : block.getSubBlocks()){
            if(sub instanceof ImportBlock && ((ImportBlock)sub).fileName.equals(className.getToken())){
                return ((ImportBlock)sub).directory;
            }
        }
        return "";
    }

    @Override
    public String getName() {
        return variableName.getToken();
    }

    @Override
    public String getValue() {
        return null;
    }

    @Override
    public String getType() {
        return className.getToken();
    }

    @Override
    public String getOpeningCode() {
        return "";
    }

    @Override
    public String getBodyCode() {
        return "mv.visitTypeInsn(NEW, \""+directory+"/"+className+"\");\n" +
                "mv.visitInsn(DUP);\n" +
                "mv.visitMethodInsn(INVOKESPECIAL, \""+directory+"/"+className+"\", \"<init>\", \"()V\", false);\n" +
                "mv.visitVarInsn(ASTORE,"+ getId() +");\n";
    }

    @Override
    public String getClosingCode() {
        return "";
    }
}
