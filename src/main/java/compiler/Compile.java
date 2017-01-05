package compiler;

import compiler.block.Block;
import compiler.block.method.ConstructorBlock;
import compiler.block.method.MethodBlock;

import java.io.*;

public class Compile {


    PrintWriter w = null;

    public Compile(Block block) {

        try {
            File file = new File("src/main/java/javassist_test/" + Constants.FILENAME + ".java");
            file.createNewFile();
            w = new PrintWriter(file);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        p("package javassist_test;");

        convertToJavassistRec(block);


        w.close();

        execute();
    }


    /**
     * Converts the block structure into ASM and saves as a .java file
     */

    public void convertToJavassistRec(Block block) {

        System.out.println(block.getClass());
        if (block instanceof MethodBlock) {
            MethodBlock b = (MethodBlock) block;

            if (b.getName().equals("main")) {
                p("\n cc.addMethod(CtNewMethod.make(\n" +
                        "                        \"public static void main(String args[]){ \"+\n");
            } else {
                p(b.getOpeningCode());
                p(b.getBodyCode());
            }

        }else {
            if(block.getOpeningCode() != null && !block.getOpeningCode().equals(""))
                p(block.getOpeningCode());
            if(block.getBodyCode() != null && !block.getBodyCode().equals(""))
                p(block.getBodyCode());
        }

        for (Block sub : block.getSubBlocks()) {
            convertToJavassistRec(sub);
        }
        if(block.getClosingCode() != null && !block.getClosingCode().equals(""))
            p(block.getClosingCode());

    }

    /**
     * Executes the main class file that is generated
     */
    public void execute() {

        for (int i = 0; i < 2; i++) {

            synchronized (this) {
                try {
                    Process process = null;

                    System.out.println("Executing...");
                    process = new ProcessBuilder("cmd.exe", "/C", Constants.FILE_LOCATIONS + Constants.MAIN_CLASS).start();

                    BufferedReader reader;

                    reader = new BufferedReader(new InputStreamReader(process.getInputStream()));

                    StringBuilder builder = new StringBuilder();
                    String line = null;

                    while ((line = reader.readLine()) != null) {
                        builder.append(line);
                        builder.append(System.getProperty("line.separator"));
                    }

                    String result = builder.toString();
                    System.out.println(result);
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        System.out.println("Complete.");
    }

    public PrintWriter p(String line) {
        w.println(line);
        return w;
    }

    public PrintWriter p() {
        w.println();
        return w;
    }
}
