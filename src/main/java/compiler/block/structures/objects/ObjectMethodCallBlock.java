package compiler.block.structures.objects;

import compiler.Parameter;
import compiler.Utils;
import compiler.block.Block;
import compiler.symbol_table.SymbolTable;

/**
 * Calling a method of an object
 */
public class ObjectMethodCallBlock extends Block {

    private String className, methodName, type;
    private Parameter[] params;

    public ObjectMethodCallBlock(Block superBlock, String className, String methodName,Parameter[] params) {
        super(superBlock, false, false);
        id = SymbolTable.getInstance().getValue(Utils.getMethod(this), className).getId();

        this.className = className;
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
                "mv.visitMethodInsn(INVOKEVIRTUAL, \"asm/"+className+"\", \""+methodName+"\", \"()V\", false);\n";
    }

    public Parameter[] getParameters() {
        return params;
    }

    @Override
    public void init() {

    }

    @Override
    public void run() {

    }

    @Override
    public String getName() {
        return className;
    }

}