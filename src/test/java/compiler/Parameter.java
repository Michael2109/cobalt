package compiler;

public class Parameter {

    private String type;
    private String name;

    // Represents what is input for ASM. E.g. int = 'I', String = 'Ljava/lang/String;'
    private String asmType;

    public Parameter(String type, String name) {
        this.type = type;
        this.name = name;


    }

    public String getName() {
        return name;
    }

    public String getType() {
        return type;
    }

    @Override
    public String toString() {
        return type + " : " + name;
    }

    public String getAsmType() {

        if (type.equals("int")) {
            return "I";
        }
        if (type.equals("String")) {
            return "Ljava/lang/String;";
        }
        return null;
    }

    public void setAsmType(String asmType) {
        this.asmType = asmType;
    }
}