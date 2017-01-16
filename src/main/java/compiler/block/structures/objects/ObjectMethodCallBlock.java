package compiler.block.structures.objects;

import compiler.Parameter;
import compiler.Utils;
import compiler.block.Block;
import compiler.block.imports.ImportBlock;
import compiler.block.packages.PackageBlock;
import compiler.block.structures.FileBlock;
import compiler.symbol_table.SymbolTable;

/**
 * Calling a method of an object
 */
public class ObjectMethodCallBlock extends Block {

    private String className, variableName, methodName, type;
    private Parameter[] params;
    private String directory = "";

    public ObjectMethodCallBlock(Block superBlock, String variableName, String methodName, Parameter[] params) {
        super(superBlock, false, false);
        id = SymbolTable.getInstance().getValue(Utils.getMethod(this), variableName).getId();
        className = SymbolTable.getInstance().getValue(Utils.getMethod(this), variableName).getType();
        this.variableName = variableName;
        this.methodName = methodName;
    }


    @Override
    public String getValue() {
        return null;
    }

    public String getType() {
        return type;
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
        return "mv.visitVarInsn(ALOAD, "+id+");\n" +
                "mv.visitMethodInsn(INVOKEVIRTUAL, \""+directory+"/"+ variableName +"\", \""+methodName+"\", \"()V\", false);\n";
    }

    public Parameter[] getParameters() {
        return params;
    }

    @Override
    public void init() {
        directory = getPackage();
    }


    // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
    public String getPackage(){
        // Get the FileBlock to find the imports
        Block block = this;
        while(!(block instanceof FileBlock)){
            block = block.getSuperBlock();
        }

        // Get the directory of the Object
        for(Block sub : block.getSubBlocks()){
            if(sub instanceof PackageBlock){
                return ((PackageBlock)sub).directory;
            }
        }
        return "";
    }

    @Override
    public String getName() {
        return variableName;
    }

}