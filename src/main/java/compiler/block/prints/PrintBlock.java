package compiler.block.prints;

import compiler.block.Block;

public class PrintBlock extends Block {

    String type = "print";
    String value;
    boolean isVariable = false;


    public PrintBlock(Block superBlock, String value, boolean printVariable) {
        super(superBlock, false, false);
        this.value = value;
        this.isVariable = printVariable;

    }

    public String getType(){
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

        if(isVariable){
            return "mv.visitFieldInsn(GETSTATIC, \"java/lang/System\", \"out\", \"Ljava/io/PrintStream;\");\n" +
                    "mv.visitLdcInsn(\""+value+"\");\n" +
                    "mv.visitMethodInsn(INVOKEVIRTUAL, \"java/io/PrintStream\", \"println\", \"(Ljava/lang/String;)V\");";
        }else{
            //return "System.out.println(\""+value+"\");";
            return "     mv.visitFieldInsn(GETSTATIC, \"java/lang/System\", \"out\", \"Ljava/io/PrintStream;\");\n" +
                    "            mv.visitLdcInsn(\""+value+"\");\n" +
                    "            mv.visitMethodInsn(INVOKEVIRTUAL, \"java/io/PrintStream\", \"println\", \"(Ljava/lang/String;)V\");";
        }


    }

    public String getValue(){
        return value;
    }

    public boolean isVariable(){
        return isVariable;
    }

    @Override
    public void init() {

    }

    @Override
    public void run() {


       // getSuperBlock().addVariable(new Variable(getSuperBlock(), t, name, value));
    }

    @Override
    public String getName() {
        return null;
    }
}
