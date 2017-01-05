package compiler.block.structure;

import compiler.block.Block;

/**
 * Represents a class.
 */
public class ClassBlock extends Block {
	
	private String name;

	public ClassBlock(Block superBlock, String name) {
		super(superBlock, true, false);
		this.name = name;

	}
	
	public String getName() {
		return name;
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
		return   "import javassist.*;\n" +
				"import java.io.IOException;\n" +
				"\n" +
				"public class "+name+" {\n" +
				"\n" +
				"    public static void main(String args[]) throws CannotCompileException, NotFoundException, IOException {\n" +
				"\n" +
				"        ClassPool pool = ClassPool.getDefault();\n" +
				"        CtClass cc = pool.makeClass(\""+name+"\");\n"
			;

	}

	@Override
	public String getClosingCode() {
		return "    cc.writeFile(\"build/classes/main\");\n" +
				"\n" +
				"        cc.detach();}}";
	}

	@Override
	public String getBodyCode() {
		return "";
	}


	@Override
	public void run() {

	}
}