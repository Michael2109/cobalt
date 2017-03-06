/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Cobalt
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package compiler.runtime

import java.io.File

import compiler.structure.blocks.Block
import compiler.structure.blocks.packages.PackageBlock
import compiler.structure.blocks.structures.FileBlock
import compiler.structure.parsers.imports.ImportParser
import compiler.structure.parsers.packages.PackageParser
import compiler.symbol_table.SymbolTable
import compiler.tokenizer.Tokenizer
import compiler.utilities.{Constants, Utils}

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Parses the code to create the AST structure.
  */
class Runtime(sourceFile: File, outputFile: File, buildDir: File) {

  def parseFile() {


    // Read all lines from source file
    val allLines = Source.fromFile(sourceFile).getLines().toList.filter(!_.trim.isEmpty)

    // get all lines excluding comments
    val lines = getIgnoreComments(allLines)

    lines.foreach(println(_))
    // get the file
    val fileBlock: FileBlock = new FileBlock(sourceFile.getName, buildDir)

    // Get the package
    val packageBlock: PackageBlock = getPackage(lines)
    fileBlock.addBlock_=(packageBlock)

    // Get the imports
    val importBlocks: ListBuffer[Block] = getImports(lines).to[ListBuffer]
    fileBlock.addBlocks_=(importBlocks)

    // Create the block structure
    // todo change to List of blocks
    // todo if package not found use "0" not "1"
    val block: Block = getBlocks(fileBlock, lines(1 + importBlocks.size))


    // todo if package isnt found use "1" not "2"
    getBlockStructure(lines.drop(2 + importBlocks.size), block, 0)

    // Add the block to a fileblock and set fileblock as parent
    fileBlock.addBlock_=(block)
    block.superBlock_=(fileBlock)

    println("Print block info")
    Utils.printBlockInfo(fileBlock, 0)

    // Print the symbol table
    SymbolTable.getInstance.printSymbols()

    println("Compiling")
    new Compile(outputFile, fileBlock)

  }

  /**
    * Loops through the lines to get the package block
    *
    * @param lines
    * @return PackageBlock
    */
  private def getPackage(lines: List[String]): PackageBlock = {
    val parser: PackageParser = new PackageParser
    lines.filter(parser.shouldParse(_)).map((s: String) => parser.parse(null, new Tokenizer(s))).find(_.isInstanceOf[PackageBlock]).getOrElse(new PackageBlock(""))
  }

  /**
    * Loops through the lines to get the import blocks
    *
    * @param lines
    * @return List[ImportBlock]
    */
  private def getImports(lines: List[String]): List[Block] = {
    val parser: ImportParser = new ImportParser;
    lines.filter(parser.shouldParse(_)).map((s: String) => parser.parse(null, new Tokenizer(s)))
  }

  /**
    * Combines all lines, removes comments, splits into list again
    *
    * @param lines
    * @return
    */
  private def getIgnoreComments(lines: List[String]): List[String] = lines.mkString("\n").replaceAll("(?sm)(^(?:\\s*)?((?:/\\*(?:\\*)?).*?(?<=\\*/))|(?://).*?(?<=$))", "").split("\n").filter(_.trim != "").toList

  /**
    * Gets an array of blocks from a string and sets the superblock
    *
    * @param superBlock
    * @param line
    * @return
    */
  // todo make return one block instead of list. Add extra blocks to an "expressions" list in the block
  private def getBlocks(superBlock: Block, line: String): Block = {

    println(line)
    val result: ListBuffer[Block] = ListBuffer[Block]()

    var previousLineLeft = ""
    // var tuple = null
    var lineLeft: String = line

    while (lineLeft != "" && lineLeft != previousLineLeft) {
      var found = false
      previousLineLeft = lineLeft

      for (parser <- Constants.parsers) {
        if (!found) {

          lineLeft = lineLeft.trim

          if (parser.shouldParse(lineLeft)) {


            // Get the regex that matched
            val regex: String = parser.getRegexs.find(_.r.findFirstIn(lineLeft).nonEmpty).getOrElse("")

            // Get the section of the line that matched the regex
            val first: String = regex.r.findFirstIn(lineLeft).getOrElse("").trim

            // If the line started with the section then parse it
            if (lineLeft.trim.startsWith(first)) {
              found = true

              if (result.size > 0) {
                result(0).expressions += parser.parse(superBlock, new Tokenizer(first))
              } else {
                result += parser.parse(superBlock, new Tokenizer(first))
              }
              lineLeft = lineLeft.replace(first, "").trim
            }
          }
        }
      }
      if (!found) {
        throw new RuntimeException("Error parsing: '" + lineLeft + "'")
      }
    }
    println(result(0))
    result(0)

  }

  /**
    * Recursively gets the class AST structure.
    */
  private def getBlockStructure(lines: List[String], block: Block, previousIndentation: Int) {
    if (lines.size > 0) {

      val line = lines(0)
      val nextBlock = getBlocks(block, lines(0))
      val nextIndentation: Int = Utils.getIndentation(lines(0))

      // Indent + 1
      if (nextIndentation - previousIndentation == 1) {
        block.addBlock_=(nextBlock)
        getBlockStructure(lines.drop(1), nextBlock, Utils.getIndentation(line))

      } else if (previousIndentation == nextIndentation) {
        block.superBlock.addBlock_=(nextBlock)
        getBlockStructure(lines.drop(1), nextBlock, Utils.getIndentation(line))

      } else {
        var currentBlock = block.superBlock
        var i = 0
        while (i < previousIndentation - nextIndentation) {
          currentBlock = currentBlock.superBlock
          i += 1
        }
        currentBlock.addBlock_=(nextBlock)
        getBlockStructure(lines.drop(1), nextBlock, Utils.getIndentation(line))

      }

    }
  }

  /**
    * Add a row to the symbol table
    *
    * @param currentBlock
    * @param lineNumber
    */
  private def addRowSymbolTable(currentBlock: Block, lineNumber: Int): Unit = {
    // if (!currentBlock.isInstanceOf[AddBlock] && !currentBlock.isInstanceOf[SubtractBlock] && !currentBlock.isInstanceOf[MultiplyBlock] && !currentBlock.isInstanceOf[DivideBlock] && !currentBlock.isInstanceOf[IfBlock] && !currentBlock.isInstanceOf[WhileBlock] && !currentBlock.isInstanceOf[PrintBlock] && !currentBlock.isInstanceOf[ObjectMethodCallBlock] && !currentBlock.isInstanceOf[PushBlock]) if (SymbolTable.getInstance.exists(currentBlock.getName, methodName, className)) {
    //   throw new DeclarationException("Line: " + lineNumber + " " + currentBlock.getName + " has already been defined." + line)
    // }
    //  else {
    //   SymbolTable.getInstance.addRow(new Row().setId(currentBlock.id).setName(currentBlock.getName).setType(currentBlock.getType).setValue(currentBlock.getValue).setMethodName(methodName).setClassName(className))
    // }
  }





}
