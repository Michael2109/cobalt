package compiler;

import compiler.block.imports.ImportBlock;
import compiler.block.packages.PackageBlock;
import compiler.block.structures.methods.MethodBlock;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

/**
 * Creates the output file.
 * Loops through the blocks calling methods to generate the code.
 */
public class Compile {


    PrintWriter w = null;

    public Compile(File outputFile, PackageBlock packageBlock, List<ImportBlock> imports, Block block) {

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
    }

    // Initialises all blocks.
    // Allows for initialisation when all blocks have been loaded.
    public void initBlocks(Block block) {
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
            p(b.getOpeningCode());
            p(b.getBodyCode());

        } else {
            if (block.getOpeningCode() != null && !block.getOpeningCode().equals(""))
                p(block.getOpeningCode());
            if (block.getBodyCode() != null && !block.getBodyCode().equals(""))
                p(block.getBodyCode());
        }

        for (Block sub : block.getSubBlocks()) {
            generateASM(sub);
        }
        if (block.getClosingCode() != null && !block.getClosingCode().equals(""))
            p(block.getClosingCode());

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
