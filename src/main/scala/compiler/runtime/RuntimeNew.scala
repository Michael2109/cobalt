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
import compiler.tokenizer.Tokenizer
import compiler.utilities.Constants

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Parses the code to create the AST structure.
  */
class RuntimeNew(sourceFile: File, outputFile: File, buildDir: File) {


  parseFile()

  def parseFile() {


    // Read all lines from source file
    val allLines = Source.fromFile(sourceFile).getLines().toList.filter(!_.trim.isEmpty)

    // get all lines excluding comments
    val lines = getIgnoreComments(allLines)

    // get the file
    val fileBlock: FileBlock = new FileBlock(sourceFile.getName, buildDir)

    // Get the package
    val packageBlock: PackageBlock = getPackage(lines)
    fileBlock.addBlock_=(packageBlock)

    // Get the imports
    val importBlocks: ListBuffer[Block] = getImports(lines).to[ListBuffer]
    fileBlock.addBlocks_=(importBlocks)

    // Create the block structure


  }

  /**
    * Loops through the lines to get the package block
    *
    * @param lines
    * @return PackageBlock
    */
  def getPackage(lines: List[String]): PackageBlock = {
    val parser: PackageParser = new PackageParser
    lines.filter(parser.shouldParse(_)).map((s: String) => parser.parse(null, new Tokenizer(s))).find(_.isInstanceOf[PackageBlock]).getOrElse(new PackageBlock(""))
  }

  /**
    * Loops through the lines to get the import blocks
    *
    * @param lines
    * @return List[ImportBlock]
    */
  def getImports(lines: List[String]): List[Block] = {
    val parser: ImportParser = new ImportParser;
    lines.filter(parser.shouldParse(_)).map((s: String) => parser.parse(null, new Tokenizer(s)))
  }

  /**
    * Combines all lines, removes comments, splits into list again
    *
    * @param lines
    * @return
    */
  def getIgnoreComments(lines: List[String]): List[String] = lines.mkString("\n").replaceAll("((['\"])(?:(?!\\2|\\\\).|\\\\.)*\\2)|\\/\\/[^\\n]*|\\/\\*(?:[^*]|\\*(?!\\/))*\\*\\/", "").split("\n").filter(_.trim != "").toList

  def getBlock(superBlock: Block, line: String): (Block, String) = {
    for (parser <- Constants.parsers) {
      if (parser.shouldParse(line)) {
        parser.parse(superBlock, new Tokenizer(line))
      }

    }
    null
  }
}
