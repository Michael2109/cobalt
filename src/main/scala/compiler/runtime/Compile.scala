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

import java.io._

import compiler.structure.blocks.Block
import compiler.structure.blocks.structures.methods.MethodBlock


/**
  * Creates the output file.
  * Loops through the blocks calling methods to generate the code.
  */
class Compile(val outputFile: File, val block: Block) {

  println(outputFile.getAbsolutePath)
  new File(outputFile.getParent).mkdirs()
  outputFile.createNewFile
  val w: PrintWriter = new PrintWriter(outputFile)

  System.out.println("Output File: " + outputFile.getAbsolutePath)

  println("Generating ASM code...")
  generateASM(block)

  println("Complete.")
  w.close()

  // Initialises all blocks.
  // Allows for initialisation when all blocks have been loaded.
  /*
  def initBlocks(block: Block) {
    block.init()
    for (sub <- block.subBlocks) {
      initBlocks(sub)
    }
  }*/

  /**
    * Converts the blocks structure into ASM and saves as a .java file
    */
  def generateASM(block: Block) {

    block match {
      case b: MethodBlock =>
        p(b.getOpeningCode)
      case _ =>
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