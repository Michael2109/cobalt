package compiler;

import compiler.block.Block;
import compiler.block.ifs.IfBlock;
import compiler.block.loops.WhileBlock;
import compiler.block.structures.methods.MethodBlock;
import compiler.block.operators.AddBlock;
import compiler.block.operators.DivideBlock;
import compiler.block.operators.MultiplyBlock;
import compiler.block.operators.SubtractBlock;
import compiler.block.structures.classes.ClassBlock;
import compiler.exceptions.ContainerException;
import compiler.exceptions.DeclarationException;
import compiler.exceptions.IndentationException;
import compiler.parser.*;
import compiler.parser.comments.CommentParser;
import compiler.parser.ifs.IfParser;
import compiler.parser.imports.ImportParser;
import compiler.parser.loops.ForParser;
import compiler.parser.loops.WhileParser;
import compiler.parser.operators.AddParser;
import compiler.parser.operators.DivideParser;
import compiler.parser.operators.MultiplyParser;
import compiler.parser.operators.SubtractParser;
import compiler.parser.primitives.*;
import compiler.parser.prints.PrintParser;
import compiler.parser.structures.*;
import compiler.parser.structures.classes.ClassParser;
import compiler.parser.structures.methods.MethodParser;
import compiler.symbol_table.Row;
import compiler.symbol_table.SymbolTable;
import compiler.tokenizer.Tokenizer;

import java.io.*;

/**
 * Parses the file generating the block structure.
 * Outputs the block structure.
 * Compiles the block structure.
 */
public class Runtime {

    Parser[] parsers = {
            // ClassBlock Parser
            new ClassParser(),
            // MethodBlock Parser
            new MethodParser(),
            // Operator Parsers
            new AddParser(),
            new DivideParser(),
            new MultiplyParser(),
            new SubtractParser(),
            // Primitive Parsers
            new BooleanParser(),
            new CharacterParser(),
            new DoubleParser(),
            new FloatParser(),
            new IntegerParser(),
            // IfBlock Parser
            new IfParser(),
            // PrintBlock Parser
            new PrintParser(),
            new ForParser(),
            new CommentParser(),
            new MethodCallParser(),
            new ImportParser(),
            new WhileParser(),
            new ObjectParser(),
            new ObjectMethodCallParser()
    };

    Block block;
    Tokenizer tokenizer;

    //start at 1 to ignore class declaration line
    int lineNumber = 0;
    boolean noParse = false;

    public Runtime(File sourceFile, File outputFile) {

        BufferedReader br = null;
        try {

            // create a buffered reader
            br = new BufferedReader(new FileReader(sourceFile));
            String line;
            while ((line = br.readLine()) != null) {
                for (Parser parser : parsers) {
                    if (parser.shouldParse(line.trim())) {
                        tokenizer = new Tokenizer(line);
                        block = parser.parse(null,tokenizer);
                        createBlock(block, br, 0);
                    }
                }

                if (noParse) throw new compiler.exceptions.ParseException("Line: " + lineNumber);

            }

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (br != null) br.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        //    System.out.println(((ClassBlock)block).getName());
        printBlockInfo(block, 0);
//printBlocks(block,0);


        SymbolTable.getInstance().printSymbols();

        new Compile(outputFile, block);

    }


    String methodName = null;
    String className = null;
    public Block createBlock(Block currentBlock, BufferedReader br, int indentation) {
        String line = "";

        try {
            if ((line = br.readLine()) != null) {
                lineNumber++;
                if (line.trim().equals("")) {
                    createBlock(currentBlock, br, indentation);
                    return null;
                }
                int currentIndentation = getIndentation(line);
                if (currentIndentation - indentation > 1) {

                    System.out.println("Line: " + line + " Current Indentation: " + currentIndentation + " : Indentation: " + indentation);
                    throw new IndentationException("Line: " + lineNumber + "    Indentation: " + (currentIndentation - indentation));
                }
                line = line.trim();
                tokenizer = new Tokenizer(line);

                if (currentBlock instanceof MethodBlock) {
                    methodName = currentBlock.getName();
                }
                if (currentBlock instanceof ClassBlock) {
                    className = currentBlock.getName();
                }
                // Check if the next symbol exists. If so then throw and error. If not then add to the symbol table.
                if (!(currentBlock instanceof AddBlock) && !(currentBlock instanceof SubtractBlock) && !(currentBlock instanceof MultiplyBlock) && !(currentBlock instanceof DivideBlock) && !(currentBlock instanceof IfBlock) && !(currentBlock instanceof WhileBlock)) {
                    if (SymbolTable.getInstance().exists(currentBlock.getName(), methodName, className)) {
                        System.out.println(currentBlock.getName() + " " + methodName + " " + className);
                        throw new DeclarationException("Line: " + lineNumber + " " + currentBlock.getName() + " has already been defined.");
                    } else {
                        SymbolTable.getInstance().addRow(new Row().setId(currentBlock.getId()).setName(currentBlock.getName()).setType(currentBlock.getType()).setValue(currentBlock.getValue()).setMethodName(methodName).setClassName(className));

                    }
                }
                // Indented out one
                if (currentIndentation == indentation + 1) {

                    if (!currentBlock.isContainer()) {
                        throw new ContainerException("Line: " + lineNumber + "    Indentation: " + indentation + " " + currentBlock + " Block cannot store other blocks.");
                    }
                    for (Parser parser : parsers) {
                        if (parser.shouldParse(line)) {
                            Block nextBlock = parser.parse(currentBlock, tokenizer);
                            currentBlock.addBlock(nextBlock);
                            createBlock(nextBlock, br, currentIndentation);
                        }
                    }
                }
                // Indentation the same
                else if (indentation == currentIndentation) {
                    if (currentBlock.isContainer()) {
                        throw new ContainerException("Line: " + lineNumber + "    Indentation: " + indentation + " " + currentBlock + " Minimum of one block stored within container.");
                    }
                    for (Parser parser : parsers) {
                        if (parser.shouldParse(line)) {
                            Block nextBlock = parser.parse(currentBlock.getSuperBlock(), tokenizer);
                            currentBlock.getSuperBlock().addBlock(nextBlock);
                            createBlock(nextBlock, br, currentIndentation);
                        }
                    }
                }
                // Indentation decreases by any amount
                else {
                    for (Parser parser : parsers) {
                        if (parser.shouldParse(line)) {
                            currentBlock = currentBlock.getSuperBlock();
                            for (int i = 0; i < indentation - currentIndentation; i++) {
                                currentBlock = currentBlock.getSuperBlock();
                            }
                            Block nextBlock = parser.parse(currentBlock, tokenizer);
                            currentBlock.addBlock(nextBlock);
                            createBlock(nextBlock, br, currentIndentation);
                        }
                    }
                }

                if (noParse) throw new compiler.exceptions.ParseException("Line: " + lineNumber);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }


    // Prints block information
    public void printBlockInfo(Block block, int indentation) {
        String indentationString = "";
        for (int i = 0; i < indentation; i++) {
            indentationString += "    ";
        }

        System.out.println(indentationString + block.toString());

        for (Block sub : block.getSubBlocks()) {
            printBlockInfo(sub, indentation + 1);
        }
    }

    public int getIndentation(String line) {
        int amount = 0;
        int indentation = 0;
        for (char character : line.toCharArray()) {
            if (character != ' ') {
                return indentation;
            } else {
                amount++;
                if (amount % 4 == 0) {
                    indentation++;
                }
            }
        }
        return indentation;
    }

    public static void main(String args[]) {
        final Object LOCK = new Object();
        synchronized (LOCK) {
            if (args.length == 2)
                new Runtime(new File(args[0]), new File(args[1]));
            else
                new Runtime(new File("C:\\Users\\Michael\\Desktop\\JVM Compiler\\compiled\\MyCode.mlg"), new File(
                        "C:\\Users\\Michael\\Desktop\\JVM Compiler\\src\\main\\java\\asm\\GeneratedAsmCode.java"));
        }
    }

}
