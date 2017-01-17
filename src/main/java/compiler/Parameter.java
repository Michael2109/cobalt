package compiler;

public class Parameter {

	private String type;
	private String name;

	// Represents what is input for ASM. E.g. int = 'I', String = 'Ljava/lang/String;'
	private String asmType;

	public Parameter(String type, String name) {
		this.setType(type);
		this.name = name;


	}
	
	public String getName() {
		return name;
	}
	
	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	@Override
	public String toString(){
		return getType() + " : " + name;
	}

	public String getAsmType() {

		if (getType().equals("int")) {
			return  "I";
		}
		if (getType().equals("String")) {
			return "Ljava/lang/String;";
		}
		return null;
	}
}