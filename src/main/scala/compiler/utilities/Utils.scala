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

package compiler.utilities

import java.io.File

import compiler.structure.blocks.Block
import compiler.structure.blocks.imports.ImportBlock
import compiler.structure.blocks.packages.PackageBlock
import compiler.structure.blocks.structures.FileBlock
import compiler.structure.blocks.structures.kinds.{ClassBlock, ObjectBlock}
import compiler.structure.blocks.structures.methods.{ConstructorBlock, MethodBlock}

import scala.util.matching.Regex

object Utils {

  /**
    * Returns the method a blocks is within
    *
    * @param block
    * @return
    */
  def getClass(block: Block): Block = {
    var result: Block = block
    while (!(result.isInstanceOf[ClassBlock] || result.isInstanceOf[ObjectBlock])) {
      {
        if (block.superBlock == null) {
          return null
        }
        result = result.superBlock
        if (result == null)
          return result
      }
    }
    return result
  }

  /**
    * Returns the method a blocks is within
    *
    * @param block
    * @return
    */
  def getMethod(block: Block): Option[Block] = {
    var result: Block = block
    while (!(result.isInstanceOf[MethodBlock] || result.isInstanceOf[ConstructorBlock])) {
      {
        if (block.superBlock == null) {
          return Option.empty[Block]
        }
        result = result.superBlock
        if (result == null)
          return Option.apply(result)
      }
    }
    return Option.apply(result)
  }

  def getFileBlock(blockInit: Block) : Block = {

    val fileBlock:Block = {
      var block: Block = blockInit
      while(!block.isInstanceOf[FileBlock]){
        block = block.superBlock
      }
      block
    }
    fileBlock
  }

  /**
    * Gets the PackageBlock
    * @param block
    * @return
    */
  def packageBlock(block: Block): PackageBlock = Utils.getFileBlock(block).subBlocks.find(_.isInstanceOf[PackageBlock]).getOrElse(new PackageBlock("")).asInstanceOf[PackageBlock]

  /**
    * Gets the directory of the class using the Imports. Otherwise assumes class is  in the same package
    * @param block
    * @return
    */
  def getDirectory(blockInit: Block, className: String): String = {
    // Get the FileBlock to find the imports
    var block = blockInit
    while (!(block.isInstanceOf[FileBlock])) {
      {
        block = block.superBlock
      }
    }
    // Get the directory of the Object
    for (sub <- block.subBlocks) {
      if (sub.isInstanceOf[ImportBlock] && (sub.asInstanceOf[ImportBlock]).fileName == className) {
        return (sub.asInstanceOf[ImportBlock]).directory
      }
    }
    return ""
  }

  /**
    * Gets the directory of the class using the package. Otherwise assumes class is  in the same package
    * @return
    */
  def getPackage(blockInit: Block): String = {
    // Get the FileBlock to find the imports
    var block: Block = blockInit
    while (!(block.isInstanceOf[FileBlock])) {
      {
        block = block.superBlock
      }
    }
    // Get the directory of the Object
    for (sub <- block.subBlocks) {
      if (sub.isInstanceOf[PackageBlock]) {
        return (sub.asInstanceOf[PackageBlock]).directory
      }
    }
    return ""
  }

  /**
    * Returns whether a int value, string value, or variable reference
    * int value = 0
    * string value = 1
    * variable ref = 2
    *
    * @param name
    * @return
    */
  def getType(name: String): Int = {

    if (name.charAt(0).isDigit) return 0

    if (name.startsWith("\"")) return 1

    return 2
  }

  /**
    * Returns the indentation of the blocks
    *
    * @param line
    * @return
    */
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

  /**
    * Prints blocks information
    *
    * @param block
    * @param indentation
    */
  def printBlockInfo(block: Block, indentation: Int = 0) {
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

  def isClass(blockInit : Block) : Boolean = {

      var block: Block = blockInit
      while (!block.isInstanceOf[ClassBlock] && !block.isInstanceOf[ObjectBlock]) {
        block = block.superBlock
      }

      if (block.isInstanceOf[ClassBlock]) {
        true
      }else {
        false
      }

  }

  /**
    * Returns a list of all files in the directory
    * @param dir
    * @return
    */
  def recursiveListFiles(f: File, r: Regex): Array[File] = {
    val these = f.listFiles
    val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_,r))
  }

}
