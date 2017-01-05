package compiler.symbol_table;

public class Row {

    private String type;
    private String name;
    private String value;
    private String methodName;
    private String className;

    public String getType() {
        return type;
    }

    public Row setType(String type) {
        this.type = type;
        return this;
    }

    public String getName() {
        return name;
    }

    public Row setName(String name) {
        this.name = name;
        return this;
    }

    public String getValue() {
        return value;
    }

    public Row setValue(String value) {
        this.value = value;
        return this;
    }

    public String getMethodName() {
        return methodName;
    }

    public Row setMethodName(String methodName) {
        this.methodName = methodName;
        return this;
    }

    public String getClassName() {
        return className;
    }

    public Row setClassName(String className) {
        this.className = className;
        return this;
    }
}
