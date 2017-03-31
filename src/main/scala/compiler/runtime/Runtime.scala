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

import compiler.ast.blocks.Block
import compiler.ast.blocks.packages.PackageBlock
import compiler.ast.blocks.structures.FileBlock
import compiler.ast.parsers.imports.ImportParser
import compiler.ast.parsers.packages.PackageParser
import compiler.symbol_table.SymbolTable
import compiler.tokenizer.Tokenizer
import compiler.utilities.Utils

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Parses the code to create the AST ast.
  */
class Runtime(sourceFile: File, outputFile: File, buildDir: File) {

  def parseFile() {

    // get all lines excluding comments
    val lines = getIgnoreComments(Source.fromFile(sourceFile).getLines().toList)

    // get the file
    val fileBlock: FileBlock = new FileBlock(sourceFile.getName, buildDir)

    // Get the package
    val packageBlock: PackageBlock = getPackage(lines)
    fileBlock.addBlock_=(packageBlock)

    // Get the imports
    val importBlocks: ListBuffer[Block] = getImports(lines).to[ListBuffer]
    fileBlock.addBlocks_=(importBlocks)

    // get the AST
    getBlockStructure(lines, fileBlock, -1, 0)

    // Output the result
    Utils.printBlockInfo(fileBlock)

    // Print the symbol table
    SymbolTable.getInstance.printSymbols()

    // Generate the bytecode in the output file
    new Compile(outputFile, fileBlock)
  }

  /**
    * Loops through the lines to get the package block
    *
    * @param lines A list of all file lines
    * @return PackageBlock
    */
  private def getPackage(lines: List[String]): PackageBlock = {
    val parser: PackageParser = new PackageParser
    lines.filter(parser.shouldParse).map((s: String) => parser.parse(null, new Tokenizer(s))).find(_.isInstanceOf[PackageBlock]).getOrElse(new PackageBlock(""))
  }

  /**
    * Loops through the lines to get the import blocks
    *
    * @param lines A List of all lines
    * @return List[ImportBlock]
    */
  private def getImports(lines: List[String]): List[Block] = {
    val parser: ImportParser = new ImportParser
    lines.filter(parser.shouldParse).map((s: String) => parser.parse(null, new Tokenizer(s)))
  }

  /**
    * Combines all lines, removes comments, splits into list again
    *
    * @param lines A List of all file lines
    * @return
    */
  private def getIgnoreComments(lines: List[String]): List[String] = lines.mkString("\n").replaceAll("(?sm)(^(?:\\s*)?((?:/\\*(?:\\*)?).*?(?<=\\*/))|(?://).*?(?<=$))", "").split("\n").filter(_.trim != "").toList

  /**
    * Recursively gets the class AST ast.
    */
  private def getBlockStructure(lines: List[String], block: Block, previousIndentation: Int, currentLine: Int) {
    if (lines.nonEmpty) {

      if (currentLine < lines.size) {

        val line = lines(currentLine)

        val nextBlock = Utils.getBlocks(block, line, currentLine)

        val nextIndentation: Int = Utils.getIndentation(line)

        // Indent + 1
        if (nextIndentation - previousIndentation == 1) {

          block.addBlock_=(nextBlock)
          getBlockStructure(lines, nextBlock, Utils.getIndentation(line), currentLine + 1)

        } else if (previousIndentation == nextIndentation) {
          block.superBlock.addBlock_=(nextBlock)
          getBlockStructure(lines, nextBlock, Utils.getIndentation(line), currentLine + 1)


        } else {
          var currentBlock = block.superBlock
          var i = 0
          while (i < previousIndentation - nextIndentation) {
            currentBlock = currentBlock.superBlock
            i += 1
          }
          currentBlock.addBlock_=(nextBlock)
          getBlockStructure(lines, nextBlock, Utils.getIndentation(line), currentLine + 1)

        }

      }
    }
  }
}
