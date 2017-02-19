package compiler.runtime

import java.io._

import compiler.{Compile, Utils}
import compiler.block.Block
import compiler.block.ifs.IfBlock
import compiler.block.imports.ImportBlock
import compiler.block.loops.WhileBlock
import compiler.block.operators.{AddBlock, DivideBlock, MultiplyBlock, SubtractBlock}
import compiler.block.packages.PackageBlock
import compiler.block.prints.PrintBlock
import compiler.block.structures.{FileBlock, ObjectMethodCallBlock}
import compiler.block.structures.kinds.ClassBlock
import compiler.block.structures.methods.MethodBlock
import compiler.exceptions.{ContainerException, DeclarationException, IndentationException}
import compiler.parser.Parser
import compiler.parser.comments.CommentParser
import compiler.parser.ifs.IfParser
import compiler.parser.imports.ImportParser
import compiler.parser.loops.{ForParser, WhileParser}
import compiler.parser.modifiers.ModifierParser
import compiler.parser.operators.{AddParser, DivideParser, MultiplyParser, SubtractParser}
import compiler.parser.packages.PackageParser
import compiler.parser.primitives._
import compiler.parser.prints.PrintParser
import compiler.parser.structures.{MethodCallParser, ObjectDefinitionParser, ObjectMethodCallParser}
import compiler.parser.structures.kinds.{ClassParser, ObjectParser}
import compiler.parser.structures.methods.MethodParser
import compiler.symbol_table.{Row, SymbolTable}
import compiler.tokenizer.Tokenizer

import scala.collection.JavaConverters._

class Runtime {
  private val parsers: Array[Parser[_]] = Array(

    new ModifierParser,
    // MethodBlock Parser
    new MethodParser,
    // Operator Parsers
    new AddParser,
    new DivideParser,
    new MultiplyParser,
    new SubtractParser,
    // Primitive Parsers
    new BooleanParser,
    new CharacterParser,
    new DoubleParser,
    new FloatParser,
    new IntegerParser,
    new LongParser,
    new StringParser,
    new ShortParser,
    // IfBlock Parser
    new IfParser,
    // PrintBlock Parser
    new PrintParser,
    new ForParser,
    new CommentParser,
    new MethodCallParser,
    new ImportParser,
    new WhileParser,
    new ObjectDefinitionParser,
    new ObjectMethodCallParser,
    new PackageParser,
    // Kinds Parsers
    new ObjectParser,
    new ClassParser
  )

  private var block: Block = null

  //start at 1 to ignore class declaration line

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

        if (line.trim == "")
          line = br.readLine()

        for (parser <- parsers) {

          if (parser.shouldParse(line.trim)) {
            val tokenizer: Tokenizer = new Tokenizer(line)
            block = parser.parse(null, tokenizer)
            if (block.isInstanceOf[PackageBlock]) {
              packageBlock = block.asInstanceOf[PackageBlock]
            }
            else if (block.isInstanceOf[ImportBlock]) {
              imports.add(block.asInstanceOf[ImportBlock])
            }
            else {
              readImports = true
              createBlock(block, br)
            }
          }
        }

        val fileBlock: Block = new FileBlock(sourceFile.getName)
        block.superBlock_$eq(fileBlock)
        if (packageBlock != null) fileBlock.addBlock_$eq(packageBlock)

        for (importBlock <- imports.asScala) fileBlock.addBlock_$eq(importBlock)

        fileBlock.addBlock_$eq(block)
        block = fileBlock
      }
      if (br != null) br.close()

    } catch {
      case e: FileNotFoundException => e.printStackTrace()
      case e: IOException => e.printStackTrace()
    }

    // Compile
    new Compile(outputFile, block)

    // Output generated block structure
    Utils.printBlockInfo(block)

    // Output the symbol table
    SymbolTable.getInstance.printSymbols()
  }

  def createBlock(currentBlockInit: Block, br: BufferedReader, indentation: Int = 0, lineNumber: Int = 0): Block = {
    var currentBlock = currentBlockInit

    var line: String = br.readLine()

    if (line == null) {
      return null
    }

    if (line.trim == "") {
      line = br.readLine()

    }

    val currentIndentation = Utils.getIndentation(line)

    line = line.trim()
    val splitLine = line.split(";")
    if (splitLine.length > 1) {

      val t1 = new Tokenizer(splitLine(0))
      var nextBlock: Block = null
      for (parser <- parsers) {
        if (parser.shouldParse(splitLine(0).trim)) {
          nextBlock = parser.parse(currentBlock, t1)
          currentBlock.addBlock_$eq(nextBlock)
        }
      }

      val t2 = new Tokenizer(splitLine(1).trim)
      for (parser <- parsers) {
        if (parser.shouldParse(splitLine(1).trim)) {

          val inlineBlock = parser.parse(nextBlock, t2)
          nextBlock.addBlock_$eq(inlineBlock)
        }
      }

      createBlock(currentBlock, br, indentation, lineNumber + 1)

      return null
    }

    val tokenizer = new Tokenizer(line)
    if (currentBlock.isInstanceOf[MethodBlock]) methodName = currentBlock.getName
    if (currentBlock.isInstanceOf[ClassBlock]) className = currentBlock.getName
    // Check if the next symbol exists. If so then throw and error. If not then add to the symbol table.
    if (!currentBlock.isInstanceOf[AddBlock] && !currentBlock.isInstanceOf[SubtractBlock] && !currentBlock.isInstanceOf[MultiplyBlock] && !currentBlock.isInstanceOf[DivideBlock] && !currentBlock.isInstanceOf[IfBlock] && !currentBlock.isInstanceOf[WhileBlock] && !currentBlock.isInstanceOf[PrintBlock] && !currentBlock.isInstanceOf[ObjectMethodCallBlock]) if (SymbolTable.getInstance.exists(currentBlock.getName, methodName, className)) {
      println(currentBlock.getName + " " + methodName + " " + className)

      throw new DeclarationException("Line: " + lineNumber + " " + currentBlock.getName + " has already been defined." + line)
    }
    else SymbolTable.getInstance.addRow(new Row().setId(currentBlock.id).setName(currentBlock.getName).setType(currentBlock.getType).setValue(currentBlock.getValue).setMethodName(methodName).setClassName(className))


    if (currentIndentation - indentation > 1) throw new IndentationException("Line: " + lineNumber + "    Indentation: " + (currentIndentation - indentation)) {

      if (currentBlock.isInstanceOf[MethodBlock]) methodName = currentBlock.getName
      if (currentBlock.isInstanceOf[ClassBlock]) className = currentBlock.getName
      // Check if the next symbol exists. If so then throw and error. If not then add to the symbol table.
      if (!currentBlock.isInstanceOf[AddBlock] && !currentBlock.isInstanceOf[SubtractBlock] && !currentBlock.isInstanceOf[MultiplyBlock] && !currentBlock.isInstanceOf[DivideBlock] && !currentBlock.isInstanceOf[IfBlock] && !currentBlock.isInstanceOf[WhileBlock] && !currentBlock.isInstanceOf[PrintBlock] && !currentBlock.isInstanceOf[ObjectMethodCallBlock]) if (SymbolTable.getInstance.exists(currentBlock.getName, methodName, className)) {
        println(currentBlock.getName + " " + methodName + " " + className)
        throw new DeclarationException("Line: " + lineNumber + " " + currentBlock.getName + " has already been defined.")
      }
      else {
        SymbolTable.getInstance.addRow(new Row().setId(currentBlock.id).setName(currentBlock.getName).setType(currentBlock.getType).setValue(currentBlock.getValue).setMethodName(methodName).setClassName(className))
      }
    }
    // Indented out one
    else if (currentIndentation == (indentation + 1)) {

      if (!currentBlock.isContainer) throw new ContainerException("Line: " + lineNumber + "    Indentation: " + indentation + " " + currentBlock + " Block cannot store other blocks.")
      for (parser <- parsers) {
        if (parser.shouldParse(line)) {
          val nextBlock = parser.parse(currentBlock, tokenizer)
          currentBlock.addBlock_$eq(nextBlock)
          createBlock(nextBlock, br, currentIndentation, lineNumber + 1)
        }
      }
    }
    else if (currentIndentation == indentation) {
      // Indentation the same if (indentation == currentIndentation) { if (currentBlock.isContainer) throw new ContainerException("Line: " + lineNumber + "    Indentation: " + indentation + " " + currentBlock + " Minimum of one block stored within container.")
      for (parser <- parsers) {
        if (parser.shouldParse(line)) {
          val nextBlock = parser.parse(currentBlock.superBlock, tokenizer)
          currentBlock.superBlock.addBlock_$eq(nextBlock)
          createBlock(nextBlock, br, currentIndentation, lineNumber + 1)
        }
      }
    }
    else {
      // Indentation decreases by any amount
      for (parser <- parsers) {
        if (parser.shouldParse(line)) {
          currentBlock = currentBlock.superBlock
          var i = 0
          while (i < indentation - currentIndentation) {
            currentBlock = currentBlock.superBlock
            i += 1
          }
          val nextBlock = parser.parse(currentBlock, tokenizer)

          currentBlock.addBlock_$eq(nextBlock)
          createBlock(nextBlock, br, currentIndentation, lineNumber + 1)
        }
      }
    }


    null
  }
}
