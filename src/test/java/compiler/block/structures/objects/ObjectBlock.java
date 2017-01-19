package compiler.block.structures.objects;

import compiler.block.imports.ImportBlock;
import compiler.block.packages.PackageBlock;
import compiler.block.structures.FileBlock;
import compiler.block.structures.classes.ClassBlock;

// Creation of a new object and storing to a variable
public class ObjectBlock extends Block {

    String className;
    String variableName;
    String operator;
    String newKeyword;
    String initClassName;

    String directory = "";

    public ObjectBlock(Block superBlock, String className, String variableName, String operator, String newKeyword, String initClassName) {
        super(superBlock, false, true);
        this.className = className;
        this.variableName = variableName;
        this.operator = operator;
        this.newKeyword = newKeyword;
        this.initClassName = initClassName;
    }

    @Override
    public void init() {
        System.out.println("ClassName:" + className + "getClassName:" + getClassName() + ":");
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

    @Override
    public String getType() {
        return className;
    }

    @Override
    public String getOpeningCode() {
        return "";
    }

    @Override
    public String getBodyCode() {
        return "mv.visitTypeInsn(NEW, \"" + directory + (directory.equals("") ? "" : "/") + className + "\");\n" +
                "mv.visitInsn(DUP);\n" +
                "mv.visitMethodInsn(INVOKESPECIAL, \"" + directory + (directory.equals("") ? "" : "/") + className + "\", \"<init>\", \"()V\", false);\n" +
                "mv.visitVarInsn(ASTORE," + getId() + ");\n";
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
