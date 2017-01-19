package compiler.block.structures.methods;

import compiler.Constants;
import compiler.Parameter;
import compiler.block.Block;
import compiler.block.packages.PackageBlock;
import compiler.symbol_table.Row;
import compiler.symbol_table.SymbolTable;

public class MethodBlock extends Block {

    String parameterString = "";
    String localVariableString = "";
    PackageBlock packageBlock = null;
    private String name, type;
    private Parameter[] params;

    public MethodBlock(Block superBlock, String name, String type, Parameter[] params) {
        super(superBlock, true, false);

        this.name = name;
        this.type = type;
        this.params = params;


        for (Parameter parameter : params) {

            parameterString += parameter.getAsmType();

            Block.TOTAL_BLOCKS++;
            localVariableString += "mv.visitLocalVariable(\"" + parameter.getName() + "\", \"" + parameter.getAsmType() + "\", null, lMethod0, lMethod1, " + Block.TOTAL_BLOCKS + ");\n";
            SymbolTable.getInstance().addRow(new Row().setMethodName(name).setId(Block.TOTAL_BLOCKS).setName(parameter.getName()));
        }

    }

    public Parameter[] getParameters() {
        return params;
    }

    @Override
    public void init() {
        Block block = getSuperBlock().getSuperBlock();
        // Get the package the class is within
        for (Block fileSub : block.getSubBlocks()) {

            if (fileSub instanceof PackageBlock) {
                packageBlock = (PackageBlock) fileSub;
            }
        }
    }

    public String getName() {
        return name;
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
        System.out.println(name);
        if (!name.equals("main")) {
            return "   {\n" +
                    "            /* Build '" + name + "' method */\n" +
                    "            MethodVisitor mv = cw.visitMethod(\n" +

                    "                    ACC_PUBLIC,                         // public method\n" +
                    "                    \"" + name + "\",                              // name\n" +
                    "                    \"(" + parameterString + ")V\",                            // descriptor\n" +
                    "                    null,                               // signature (null means not generic)\n" +
                    "                    null);                              // exceptions (array of strings)\n" +
                    "mv.visitCode();\n" + "\n" +
                    "Label lMethod0 = new Label();\n" +
                    "mv.visitLabel(lMethod0);\n" +
                    "Label lMethod1 = new Label();\n" +
                    "mv.visitLabel(lMethod1);\n" +
                    "mv.visitLocalVariable(\"this\", \"L" + packageBlock.directory + "/" + name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n" +
                    localVariableString;
        } else {
            return "{\n" +
                    "// Main Method\n" +
                    "MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, \"main\", \"([Ljava/lang/String;)V\", null, null);\n" +
                    "mv.visitCode();\n" +
                    "Label lMethod0 = new Label();\n" +
                    "mv.visitLabel(lMethod0);\n" +
                    "Label lMethod1 = new Label();\n" +
                    "mv.visitLabel(lMethod1);\n" +
                    "mv.visitLocalVariable(\"this\", \"L" + packageBlock.directory + "/" + name + ";\", null, lMethod0, lMethod1, " + 0 + ");\n" +
                    "mv.visitLocalVariable(\"args\", \"[Ljava/lang/String;\", null, lMethod0, lMethod1, 0);";
        }
    }

    @Override
    public String getBodyCode() {
        return "";
    }

    @Override
    public String getClosingCode() {
        return "mv.visitInsn(RETURN);                      // Return integer from top of stack\n" +
                "mv.visitMaxs(0, 0);\n" +
                "mv.visitEnd();\n" +
                "}\n";
    }

    public String toString() {
        String paramString = "";
        for (Parameter parameter : params) {
            paramString += parameter.getType() + ":" + parameter.getName() + "; ";
        }
        return name + " ( " + paramString + ")";
    }


}