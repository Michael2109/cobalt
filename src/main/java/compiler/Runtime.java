package compiler;

import compiler.block.Block;
import compiler.block.PrintBlock;
import compiler.block.comments.CommentBlock;
import compiler.block.ifs.IfBlock;
import compiler.block.loops.ForBlock;
import compiler.block.loops.WhileBlock;
import compiler.block.method.ConstructorBlock;
import compiler.block.method.MethodBlock;
import compiler.block.operators.Add;
import compiler.block.operators.Divide;
import compiler.block.operators.Multiply;
import compiler.block.operators.Subtract;
import compiler.block.primitives.*;
import compiler.block.structure.ClassBlock;
import compiler.block.structure.FileBlock;
import compiler.exceptions.ContainerException;
import compiler.exceptions.DeclarationException;
import compiler.exceptions.IndentationException;
import compiler.parser.*;
import compiler.parser.operator_parsers.AddParser;
import compiler.parser.operator_parsers.DivideParser;
import compiler.parser.operator_parsers.MultiplyParser;
import compiler.parser.operator_parsers.SubtractParser;
import compiler.parser.primitive_parsers.*;
import compiler.parser.structure.ObjectMethodCallParser;
import compiler.parser.structure.StaticMethodCallParser;
import compiler.parser.structure.MethodParser;
import compiler.parser.structure.ObjectParser;
import compiler.symbol_table.Row;
import compiler.symbol_table.SymbolTable;
import compiler.tokenizer.Tokenizer;

import java.io.*;

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
            new ConstructorParser(),
            new StaticMethodCallParser(),
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
                        block = new FileBlock();
                        createBlock(block, br, -1);
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
                if (!(currentBlock instanceof Add) && !(currentBlock instanceof Subtract) && !(currentBlock instanceof Multiply) && !(currentBlock instanceof Divide) && !(currentBlock instanceof IfBlock) && !(currentBlock instanceof WhileBlock)) {
                    if (SymbolTable.getInstance().exists(currentBlock.getName(), methodName, className)) {
                        System.out.println(currentBlock.getName() + " " + methodName + " " + className);
                        throw new DeclarationException("Line: " + lineNumber + " " + currentBlock.getName() + " has already been defined.");
                    } else {
                        SymbolTable.getInstance().addRow(new Row().setId(currentBlock.getId()).setName(currentBlock.getName()).setType(currentBlock.getType()).setValue(currentBlock.getValue()).setMethodName(methodName).setClassName(className));

                    }
                }
                // Indented out one
                if (currentIndentation == indentation + 1) {

                    if (!currentBlock.isContainer) {
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
                    if (currentBlock.isContainer) {
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


    String methodName = null;
    String className = null;

    // Prints block information
    public void printBlockInfo(Block block, int indentation) {
        String indentationString = "";
        for (int i = 0; i < indentation; i++) {
            indentationString += "    ";
        }
        Row row = new Row();
        if (block instanceof ClassBlock) {
            ClassBlock b = (ClassBlock) block;
            System.out.println(indentationString + b.getName());
            className = b.getName();
        }
        if (block instanceof ConstructorBlock) {
            ConstructorBlock b = (ConstructorBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getName());
        }
        if (block instanceof MethodBlock) {
            MethodBlock b = (MethodBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getName());
            methodName = b.getName();
        }
        if (block instanceof IntegerBlock) {
            IntegerBlock b = (IntegerBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getName() + " " + b.getValue());
        }
        if (block instanceof FloatBlock) {
            FloatBlock b = (FloatBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getName() + " " + b.getValue());
        }
        if (block instanceof DoubleBlock) {
            DoubleBlock b = (DoubleBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getName() + " " + b.getValue());
        }
        if (block instanceof BooleanBlock) {
            BooleanBlock b = (BooleanBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getName() + " " + b.getValue());
        }
        if (block instanceof CharacterBlock) {
            CharacterBlock b = (CharacterBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getName() + " " + b.getValue());
        }
        if (block instanceof PrintBlock) {
            PrintBlock b = (PrintBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getValue());
        }
        if (block instanceof IfBlock) {
            IfBlock b = (IfBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getName());
        }
        if (block instanceof ForBlock) {
            ForBlock b = (ForBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getName());
        }
        if (block instanceof WhileBlock) {
            WhileBlock b = (WhileBlock) block;
            System.out.println(indentationString + b.getType() + " " + b.getName());
        }
        if (block instanceof CommentBlock) {
            CommentBlock b = (CommentBlock) block;
            System.out.println(indentationString + b.getType());
        }

        //System.out.println(indentationString + block.getClass());
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
        new Runtime(new File(args[0]), new File(args[1]));
    }

}
