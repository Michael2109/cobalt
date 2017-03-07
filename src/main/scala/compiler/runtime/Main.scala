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

import compiler.symbol_table.SymbolTable
import compiler.utilities.Utils
import org.apache.log4j.PropertyConfigurator

object Main {

  def start(args: Array[String]) {

    PropertyConfigurator.configure("src/log4j.properties");

    if (args.length == 3) {

      val fileList:Array[File] = Utils.recursiveListFiles(new File(args(0)),"cobalt".r)


      for(file <- fileList){

        SymbolTable.getInstance.rows.clear()


        val input: File = file
        val asmFile: File = new File(file.getPath.replace(args(0),args(1)).replaceAll("(\\.[^\\.]*$)", ".java"))
        val buildDir = new File(args(2))
        new Runtime(input, asmFile, buildDir).parseFile()
        println()
      }



    }
    else {
      System.out.println("Error: Input and Output file args required. Enter with file extension removed.")
    }
  }

  def main(args: Array[String]) {
    start(args)
  }
}
