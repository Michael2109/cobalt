package compiler.utilities

import java.io.File

import compiler.block.Block
import compiler.block.imports.ImportBlock
import compiler.block.packages.PackageBlock
import compiler.block.structures.FileBlock
import compiler.block.structures.kinds.{ClassBlock, ObjectBlock}
import compiler.block.structures.methods.MethodBlock

import scala.util.matching.Regex

object Utils {

  /**
    * Returns the method a block is within
    *
    * @param block
    * @return
    */
  def getMethod(block: Block): Block = {
    var result: Block = block
    while (!(result.isInstanceOf[MethodBlock])) {
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
    * Returns the indentation of the block
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
    * Prints block information
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
