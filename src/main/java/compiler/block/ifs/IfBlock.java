package compiler.block.ifs;

import compiler.Parameter;
import compiler.Utils;
import compiler.block.Block;
import compiler.symbol_table.SymbolTable;

public class IfBlock extends Block {

    private String type = "if";
    private Parameter[] params;
    String name;
    String pointer;
    String operator;
    String value;

    public IfBlock(Block superBlock, String name) {
        super(superBlock, true, false);
        this.name = name;

        String[] split = name.split(" ");

        // E.g. x < 10
        if(split.length > 1) {
            pointer = split[0];
            pointer = ""+ SymbolTable.getInstance().getValue(Utils.getMethod(this), name).getId();
            operator = split[1];
            value = split[2];
        }else{
            //boolean value
            value = name;

        }
        System.out.println("If Value: " + name);
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
        return "Label l2 = new Label();\n" +
                "mv.visitJumpInsn(IF_ICMPGE, l2);";
    }

    @Override
    public String getBodyCode() {
        return "";
    }

    @Override
    public String getClosingCode() {
        return "}";
    }



    public Parameter[] getParameters() {
        return params;
    }


    @Override
    public void run() {

    }
}