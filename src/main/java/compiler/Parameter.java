package compiler;

import compiler.block.Type;

public class Parameter {

	private String type;
	private String name;

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
}