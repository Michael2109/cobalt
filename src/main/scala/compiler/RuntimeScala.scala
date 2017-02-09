package compiler

import java.io._

import compiler.block.Block
import compiler.block.ifs.IfBlock
import compiler.block.imports.ImportBlock
import compiler.block.loops.WhileBlock
import compiler.block.operators.{AddBlock, DivideBlock, MultiplyBlock, SubtractBlock}
import compiler.block.packages.PackageBlock
import compiler.block.prints.PrintBlock
import compiler.block.structures.FileBlock
import compiler.block.structures.classes.ClassBlock
import compiler.block.structures.methods.MethodBlock
import compiler.block.structures.objects.ObjectMethodCallBlock
import compiler.exceptions.{ContainerException, DeclarationException, IndentationException, ParseException}
import compiler.parser.Parser
import compiler.parser.comments.CommentParser
import compiler.parser.ifs.IfParser
import compiler.parser.imports.ImportParser
import compiler.parser.loops.{ForParser, WhileParser}
import compiler.parser.operators.{AddParser, DivideParser, MultiplyParser, SubtractParser}
import compiler.parser.packages.PackageParser
import compiler.parser.primitives._
import compiler.parser.prints.PrintParser
import compiler.parser.structures.classes.ClassParser
import compiler.parser.structures.{MethodCallParser, ObjectMethodCallParser, ObjectParser}
import compiler.parser.structures.methods.MethodParser
import compiler.symbol_table.{Row, SymbolTable}
import compiler.tokenizer.Tokenizer

class RuntimeScala {
  private[compiler] val parsers: Array[Parser[_]] = Array(
    // MethodBlock Parser
    new MethodParser,
    // Operator Parsers
    new AddParser, new DivideParser, new MultiplyParser, new SubtractParser,
    // Primitive Parsers
    new BooleanParser, new CharacterParser, new DoubleParser, new FloatParser, new IntegerParser,
    // IfBlock Parser
    new IfParser,
    // PrintBlock Parser
    new PrintParser, new ForParser, new CommentParser, new MethodCallParser, new ImportParser, new WhileParser, new ObjectParser, new ObjectMethodCallParser, new PackageParser,
    // ClassBlock Parser
    new ClassParser)

  private var block: Block = null
  private var tokenizer: Tokenizer = null
  //start at 1 to ignore class declaration line
  private var lineNumber: Int = 0
  private var methodName: String = null
  private var className: String = null

  def this(sourceFile: File, outputFile: File) {
    this()
    var packageBlock: PackageBlock = null
    val imports: java.util.List[ImportBlock] = new java.util.ArrayList[ImportBlock]
    var br: BufferedReader = null
    try {
      // create a buffered reader
      br = new BufferedReader(new FileReader(sourceFile))
      var line: String = null
      var readImports = false
      while ((line = br.readLine) != null && !readImports) {
        for (parser <- parsers) {
          if (parser.shouldParse(line.trim)) {
            tokenizer = new Tokenizer(line)
            block = parser.parse(null, tokenizer)
            if (block.isInstanceOf[PackageBlock]) {
              packageBlock = block.asInstanceOf[PackageBlock]
            }
            else if (block.isInstanceOf[ImportBlock]) {
              imports.add(block.asInstanceOf[ImportBlock])
            }
            else {
              println("Made it")
              readImports = true
              createBlock(block, br, 0)
            }
          }
        }
        val fileBlock: Block = new FileBlock(sourceFile.getName)
        block.superBlock_$eq(fileBlock)
        if (packageBlock != null) fileBlock.addBlock_$eq(packageBlock)
        import scala.collection.JavaConversions._
        for (importBlock <- imports) fileBlock.addBlock_$eq(importBlock)

        fileBlock.addBlock_$eq(block)
        block = fileBlock
      }
      if (br != null) br.close()

    } catch {
      case e: FileNotFoundException => e.printStackTrace()
      case e: IOException => e.printStackTrace()
    }

    printBlockInfo(block, 0)

    SymbolTable.getInstance.printSymbols()

    new Compile(outputFile, block)
  }

  def createBlock(currentBlockInit: Block, br: BufferedReader, indentation: Int): Block = {
    var currentBlock = currentBlockInit

    var line: String = br.readLine()

    lineNumber += 1

    if (line != null) {


      if (line.trim() == "") {
        createBlock(currentBlock, br, indentation)
        return null
      }

      val currentIndentation = getIndentation(line)

      // println("Before: "+ line)
      line = line.trim()
      if (currentIndentation - indentation > 1) throw new IndentationException("Line: " + lineNumber + "    Indentation: " + (currentIndentation - indentation)) {
        tokenizer = new Tokenizer(line)
        if (currentBlock.isInstanceOf[MethodBlock]) methodName = currentBlock.getName
        if (currentBlock.isInstanceOf[ClassBlock]) className = currentBlock.getName
        // Check if the next symbol exists. If so then throw and error. If not then add to the symbol table.
        if (!currentBlock.isInstanceOf[AddBlock] && !currentBlock.isInstanceOf[SubtractBlock] && !currentBlock.isInstanceOf[MultiplyBlock] && !currentBlock.isInstanceOf[DivideBlock] && !currentBlock.isInstanceOf[IfBlock] && !currentBlock.isInstanceOf[WhileBlock] && !currentBlock.isInstanceOf[PrintBlock] && !currentBlock.isInstanceOf[ObjectMethodCallBlock]) if (SymbolTable.getInstance.exists(currentBlock.getName, methodName, className)) {
          System.out.println(currentBlock.getName + " " + methodName + " " + className)
          throw new DeclarationException("Line: " + lineNumber + " " + currentBlock.getName + " has already been defined.")
        }
        else {
          SymbolTable.getInstance.addRow(new Row().setId(currentBlock.id).setName(currentBlock.getName).setType(currentBlock.getType).setValue(currentBlock.getValue).setMethodName(methodName).setClassName(className))
        }
        // Indented out one
        if (currentIndentation == indentation + 1) {
          if (!currentBlock.isContainer) throw new ContainerException("Line: " + lineNumber + "    Indentation: " + indentation + " " + currentBlock + " Block cannot store other blocks.")
          for (parser <- parsers) {
            if (parser.shouldParse(line)) {
              val nextBlock: Block = parser.parse(currentBlock, tokenizer)
              currentBlock.addBlock_$eq(nextBlock)
              createBlock(nextBlock, br, currentIndentation)
            }
          }
        }
        else if (currentIndentation == indentation) {
          // Indentation the same if (indentation == currentIndentation) { if (currentBlock.isContainer) throw new ContainerException("Line: " + lineNumber + "    Indentation: " + indentation + " " + currentBlock + " Minimum of one block stored within container.")
          for (parser <- parsers) {
            if (parser.shouldParse(line)) {
              val nextBlock: Block = parser.parse(currentBlock.superBlock, tokenizer)
              currentBlock.superBlock.addBlock_$eq(nextBlock)
              createBlock(nextBlock, br, currentIndentation)
            }
          }
        }
        else {
          // Indentation decreases by any amount
          for (parser <- parsers) {
            if (parser.shouldParse(line)) {
              currentBlock = currentBlock.superBlock
              var i: Int = 0
              while (i < indentation - currentIndentation) {
                currentBlock = currentBlock.superBlock
                i += 1
              }
              val nextBlock: Block = parser.parse(currentBlock, tokenizer)
              currentBlock.addBlock_$eq(nextBlock)
              createBlock(nextBlock, br, currentIndentation)
            }
          }
        }
      }
    }
    return null
  }


  // Prints block information
  def printBlockInfo(block: Block, indentation: Int) {
    var indentationString: String = ""
    var i: Int = 0
    while (i < indentation) {
      {
        indentationString += "    "
      }
      {
        i += 1
        i - 1
      }
    }
    System.out.println(indentationString + block.toString)
    for (sub <- block.subBlocks) {
      printBlockInfo(sub, indentation + 1)
    }
  }

  def getIndentation(line: String): Int = {
    var amount: Int = 0
    var indentation: Int = 0
    for (character <- line.toCharArray) {
      if (character != ' ') return indentation
      else {
        amount += 1
        if (amount % 4 == 0) indentation += 1
      }
    }
    indentation
  }

}
