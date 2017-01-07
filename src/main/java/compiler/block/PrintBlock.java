package compiler.block;

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
            return "System.out.println("+value+");";
        }else{
            return "System.out.println(\""+value+"\");";
        }


    }

    public String getValue(){
        return value;
    }

    public boolean isVariable(){
        return isVariable;
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
