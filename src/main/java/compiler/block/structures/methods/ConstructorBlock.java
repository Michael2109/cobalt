package compiler.block.structures.methods;

import compiler.Parameter;
import compiler.block.Block;

import java.util.List;

public class ConstructorBlock extends Block {

    private List<Parameter> parameters;

    public ConstructorBlock(Block superBlock, List<Parameter> parameters) {
        super(superBlock, true, false);
        this.parameters = parameters;


    }

    @Override
    public void init() {

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
        return "";
    }

    @Override
    public String getBodyCode() {
        return "";
    }

    @Override
    public String getClosingCode() {
        return "mv.visitInsn(RETURN);                      // End the constructor method\n" +
                "mv.visitMaxs(0, 0);\n" +
                "mv.visitEnd();\n" +
                "}";
    }

    public String toString() {
        String paramString = "";
        for (Parameter parameter : parameters) {
            paramString += parameter.getType() + ":" + parameter.getName() + "; ";
        }
        return "constructor: ( " + paramString + ")";
    }
}