package compiler;

import compiler.block.Block;
import compiler.block.structures.classes.ClassBlock;
import compiler.block.structures.methods.MethodBlock;

import java.io.*;

/**
 * Creates the output file.
 * Loops through the blocks calling methods to generate the code.
 */
public class Compile {


    PrintWriter w = null;

    public Compile(File outputFile, Block block) {

        try {
            outputFile.createNewFile();
            w = new PrintWriter(outputFile);
            System.out.println("Output File: " + outputFile.getAbsolutePath());
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }



        initBlocks(block);
        generateASM(block);


        w.close();

        execute();

       // MyCode.main(new String[0]);
    }

    // Initialises all blocks.
    // Allows for initialisation when all blocks have been loaded.
    public void initBlocks(Block block){
            block.init();
            for (Block sub : block.getSubBlocks()) {
                initBlocks(sub);
            }
    }


    /**
     * Converts the block structure into ASM and saves as a .java file
     */
    public void generateASM(Block block) {


        if (block instanceof MethodBlock) {
            MethodBlock b = (MethodBlock) block;

            if (b.getName().equals("main")) {
                //p("public static void main(String args[]){ \n");
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
            generateASM(sub);
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
