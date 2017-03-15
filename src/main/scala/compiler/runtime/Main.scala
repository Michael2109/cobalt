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
import org.codehaus.janino.SimpleCompiler
import org.slf4j.{Logger, LoggerFactory}


object Main {

  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]) {
    PropertyConfigurator.configure("src/log4j.properties")
    if (args.length == 3) {
      logger.info("Program arguments: $args")
      start(args)
    } else {
      logger.info("Default program arguments used.")
      start(Array("cobalt_source", "src/main/java", "build/classes/main"))
    }
  }

  def start(args: Array[String]) {



    if (args.length == 3) {

      val fileList:Array[File] = Utils.recursiveListFiles(new File(args(0)),"cobalt".r)


      for(file <- fileList){

        SymbolTable.getInstance.rows.clear()


        val input: File = file
        val asmFile: File = new File(file.getPath.replace(args(0),args(1)).replaceAll("(\\.[^\\.]*$)", ".java"))
        val buildDir = new File(args(2))
        new Runtime(input, asmFile, buildDir).parseFile()

        val compiler = new SimpleCompiler(asmFile.getAbsolutePath);
        val loader = compiler.getClassLoader();
        val compClass = loader.loadClass(asmFile.getPath.replace(".java", "").replace("\\", ".").replace(args(1).replace("/", ".") + ".", ""));
        val instance = compClass.newInstance();
        compClass.getMethod("main", classOf[Array[String]]).invoke(null, Array[String]())

        println()
      }



    }
    else {
      System.out.println("Error: Input and Output file args required. Enter with file extension removed.")
    }
  }
}
