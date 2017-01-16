package compiler.block.structures.objects;

import compiler.Parameter;
import compiler.Utils;
import compiler.block.Block;
import compiler.block.imports.ImportBlock;
import compiler.block.packages.PackageBlock;
import compiler.block.structures.FileBlock;
import compiler.block.structures.classes.ClassBlock;
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

    public Parameter[] getParameters() {
        return params;
    }

    @Override
    public void init() {
        if (className.equals(getClassName())) {
            directory = getPackage();
        } else {
            directory = getDirectory();
        }
    }

    @Override
    public String getName() {
        return variableName;
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
    public String getBodyCode() {
        return "mv.visitVarInsn(ALOAD, " + id + ");\n" +
                "mv.visitMethodInsn(INVOKEVIRTUAL, \"" + directory + "/" + className + "\", \"" + methodName + "\", \"()V\", false);\n";
    }

    @Override
    public String getClosingCode() {
        return "";
    }

    // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
    public String getDirectory() {
        // Get the FileBlock to find the imports
        Block block = this;
        while (!(block instanceof FileBlock)) {
            block = block.getSuperBlock();
        }

        // Get the directory of the Object
        for (Block sub : block.getSubBlocks()) {
            if (sub instanceof ImportBlock && ((ImportBlock) sub).fileName.equals(className)) {
                return ((ImportBlock) sub).directory;
            }
        }
        return "";
    }

    // Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
    public String getPackage() {
        // Get the FileBlock to find the imports
        Block block = this;
        while (!(block instanceof FileBlock)) {
            block = block.getSuperBlock();
        }

        // Get the directory of the Object
        for (Block sub : block.getSubBlocks()) {
            if (sub instanceof PackageBlock) {
                return ((PackageBlock) sub).directory;
            }
        }
        return "";
    }

    // Returns the main class name for the file
    public String getClassName() {
        // Get the FileBlock to find the imports
        Block block = this;
        while (!(block instanceof ClassBlock)) {
            block = block.getSuperBlock();
        }

        // Get the directory of the Object
        return block.getName();
    }

}