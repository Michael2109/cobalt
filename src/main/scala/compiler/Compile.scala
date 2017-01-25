package compiler

import java.io._

import compiler.block.Block
import compiler.block.structures.methods.MethodBlock

/**
  * Creates the output file.
  * Loops through the blocks calling methods to generate the code.
  */
class Compile(val outputFile: File, val block: Block) {
  try {
    outputFile.createNewFile
    w = new PrintWriter(outputFile)
    System.out.println("Output File: " + outputFile.getAbsolutePath)
  }
  catch {
    case e: FileNotFoundException => {
      e.printStackTrace()
    }
    case e: IOException => {
      e.printStackTrace()
    }
  }

  println("Initialising blocks...")
  initBlocks(block)

  println("Generating ASM code...")
  generateASM(block)

  println("Complete.")
  w.close()
  private[compiler] var w: PrintWriter = null

  // Initialises all blocks.
  // Allows for initialisation when all blocks have been loaded.
  def initBlocks(block: Block) {
    block.init()
    for (sub <- block.getSubBlocks) {
      initBlocks(sub)
    }
  }

  /**
    * Converts the block structure into ASM and saves as a .java file
    */
  def generateASM(block: Block) {

    if (block.isInstanceOf[MethodBlock]) {
      val b: MethodBlock = block.asInstanceOf[MethodBlock]
      p(b.getOpeningCode)
      p(b.getBodyCode)

    }
    else {
      if (block.getOpeningCode != null && block.getOpeningCode != "") {
        p(block.getOpeningCode)
      }
      if (block.getBodyCode != null && block.getBodyCode != "") {
        p(block.getBodyCode)
      }



    }
    for (sub <- block.getSubBlocks) {
      generateASM(sub)
    }
    if (block.getClosingCode != null && block.getClosingCode != "") p(block.getClosingCode)
  }

  // Call to write using the printwriter
  def p(line: String): PrintWriter = {
    w.println(line)
    return w
  }

  // Call to write a newline using the printwriter
  def p: PrintWriter = {
    w.println()
    return w
  }
}