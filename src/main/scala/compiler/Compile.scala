package compiler

import java.io._

import compiler.block.Block
import compiler.block.structures.methods.MethodBlock
import test_classes.block.Block

/**
  * Creates the output file.
  * Loops through the blocks calling methods to generate the code.
  */
class Compile(val outputFile: File, val block: Block) {

  println(outputFile.getAbsolutePath)
  outputFile.createNewFile
  val w: PrintWriter = new PrintWriter(outputFile)

  System.out.println("Output File: " + outputFile.getAbsolutePath)

  println("Initialising blocks...")
  initBlocks(block)

  println("Generating ASM code...")
  generateASM(block)

  println("Complete.")
  w.close()

  // Initialises all blocks.
  // Allows for initialisation when all blocks have been loaded.
  def initBlocks(block: Block) {
    block.init()
    for (sub <- block.subBlocks) {
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
    }
    else {
      if (block.getOpeningCode != null && block.getOpeningCode != "") {
        p(block.getOpeningCode)
      }
    }
    for (sub <- block.subBlocks) {
      generateASM(sub)
    }
    if (block.getClosingCode != null && block.getClosingCode != "") p(block.getClosingCode)
  }

  // Call to write using the printwriter
  def p(line: String): PrintWriter = {
    w.println(line)
    w
  }

  // Call to write a newline using the printwriter
  def p: PrintWriter = {
    w.println()
    w
  }
}