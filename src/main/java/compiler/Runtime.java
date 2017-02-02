package compiler;

import compiler.block.Block;
import compiler.block.ifs.IfBlock;
import compiler.block.imports.ImportBlock;
import compiler.block.loops.WhileBlock;
import compiler.block.operators.AddBlock;
import compiler.block.operators.DivideBlock;
import compiler.block.operators.MultiplyBlock;
import compiler.block.operators.SubtractBlock;
import compiler.block.packages.PackageBlock;
import compiler.block.prints.PrintBlock;
import compiler.block.structures.FileBlock;
import compiler.block.structures.classes.ClassBlock;
import compiler.block.structures.methods.MethodBlock;
import compiler.block.structures.objects.ObjectMethodCallBlock;
import compiler.exceptions.ContainerException;
import compiler.exceptions.DeclarationException;
import compiler.exceptions.IndentationException;
import compiler.exceptions.ParseException;
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
import compiler.parser.packages.PackageParser;
import compiler.parser.primitives.*;
import compiler.parser.prints.PrintParser;
import compiler.parser.structures.*;
import compiler.parser.structures.classes.ClassParser;
import compiler.parser.structures.methods.MethodParser;
import compiler.symbol_table.Row;
import compiler.symbol_table.SymbolTable;
import compiler.tokenizer.Tokenizer;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Parses the file generating the block structure.
 * Outputs the block structure.
 * Compiles the block structure.
 */
public class Runtime {

    Parser[] parsers = {

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
            new ObjectMethodCallParser(),
            new PackageParser(),
            // ClassBlock Parser
            new ClassParser()
    };

    Block block;
    Tokenizer tokenizer;

    //start at 1 to ignore class declaration line
    int lineNumber = 0;
    boolean noParse = false;
    String methodName = null;
    String className = null;

    public Runtime(File sourceFile, File outputFile) {

        Utils.getType("1");

        PackageBlock packageBlock = null;
        List<ImportBlock> imports = new ArrayList<>();
        BufferedReader br = null;
        try {

            // create a buffered reader
            br = new BufferedReader(new FileReader(sourceFile));
            String line;
            while ((line = br.readLine()) != null) {

                for (Parser parser : parsers) {
                    if (parser.shouldParse(line.trim())) {
                        tokenizer = new Tokenizer(line);
                        block = parser.parse(null, tokenizer);
                        if (block instanceof PackageBlock) {
                            packageBlock = (PackageBlock) block;
                            continue;
                        } else if (block instanceof ImportBlock) {
                            imports.add((ImportBlock) block);
                            continue;
                        } else {
                            createBlock(block, br, 0);
                        }
                    }
                }

                Block fileBlock = new FileBlock(sourceFile.getName());
                block.superBlock_$eq(fileBlock);

                if (packageBlock != null)
                    fileBlock.addBlock_$eq(packageBlock);

                for (ImportBlock importBlock : imports)
                    fileBlock.addBlock_$eq(importBlock);

                fileBlock.addBlock_$eq(block);
                block = fileBlock;

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

        printBlockInfo(block, 0);



        new Compile(outputFile, block);

        SymbolTable.getInstance().printSymbols();
    }

    public static void main(String args[]) {
        if (args.length == 2) {
            File input = new File(args[0] + ".mlg");
            File output = new File(args[1] + ".java");
            new Runtime(input, output);
        } else {
            System.out.println("Error: Input and Output file args required.");
            return;
        }
    }

    public Block createBlock(Block currentBlock, BufferedReader br, int indentation) {
        String line = "";

        try {

            if (br.ready()) {
                line = br.readLine();
                lineNumber++;
                if (line.trim().equals("")) {
                    createBlock(currentBlock, br, indentation);
                    return null;
                }
                int currentIndentation = getIndentation(line);

                if (currentIndentation - indentation > 1) {
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
                if (!(currentBlock instanceof AddBlock) && !(currentBlock instanceof SubtractBlock) && !(currentBlock instanceof MultiplyBlock) && !(currentBlock instanceof DivideBlock) && !(currentBlock instanceof IfBlock) && !(currentBlock instanceof WhileBlock) && !(currentBlock instanceof PrintBlock) && !(currentBlock instanceof ObjectMethodCallBlock)) {
                    if (SymbolTable.getInstance().exists(currentBlock.getName(), methodName, className)) {
                        System.out.println(currentBlock.getName() + " " + methodName + " " + className);
                        throw new DeclarationException("Line: " + lineNumber + " " + currentBlock.getName() + " has already been defined.");
                    } else {
                        SymbolTable.getInstance().addRow(new Row().setId(currentBlock.id()).setName(currentBlock.getName()).setType(currentBlock.getType()).setValue(currentBlock.getValue()).setMethodName(methodName).setClassName(className));
                    }
                }

                // Indented out one
                if (currentIndentation == indentation + 1) {

                    if (!currentBlock.isContainer()) {
                        throw new ContainerException("Line: " + lineNumber + "    Indentation: " + indentation + " " + currentBlock + " Block cannot store other blocks.");
                    }
                    boolean parsable = false;
                    for (Parser parser : parsers) {
                        if (parser.shouldParse(line)) {
                            parsable = true;
                            Block nextBlock = parser.parse(currentBlock, tokenizer);
                            currentBlock.addBlock_$eq(nextBlock);
                            createBlock(nextBlock, br, currentIndentation);
                        }
                    }
                    if (!parsable)
                        throw new ParseException("Line: " + lineNumber + " " + line);

                }
                // Indentation the same
                else if (indentation == currentIndentation) {
                    if (currentBlock.isContainer()) {
                        throw new ContainerException("Line: " + lineNumber + "    Indentation: " + indentation + " " + currentBlock + " Minimum of one block stored within container.");
                    }
                    boolean parsable = false;
                    for (Parser parser : parsers) {
                        if (parser.shouldParse(line)) {
                            parsable = true;
                            Block nextBlock = parser.parse(currentBlock.superBlock(), tokenizer);
                            currentBlock.superBlock().addBlock_$eq(nextBlock);
                            createBlock(nextBlock, br, currentIndentation);
                        }
                    }
                    if (!parsable)
                        throw new ParseException("Line: " + lineNumber + " " + line);
                }
                // Indentation decreases by any amount
                else {

                    boolean parsable = false;
                    for (Parser parser : parsers) {
                        if (parser.shouldParse(line)) {
                            parsable = true;
                            currentBlock = currentBlock.superBlock();
                            for (int i = 0; i < indentation - currentIndentation; i++) {
                                currentBlock = currentBlock.superBlock();
                            }
                            Block nextBlock = parser.parse(currentBlock, tokenizer);
                            currentBlock.addBlock_$eq(nextBlock);
                            createBlock(nextBlock, br, currentIndentation);
                        }
                    }
                    if (!parsable)
                        throw new ParseException("Line: " + lineNumber);
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

        for (Block sub : block.subBlocks()) {
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

}