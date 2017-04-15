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
      val defaultArgs = Array("cobalt_source/test/assignment/AssignmentTest.cobalt", "cobalt_java/test/assignment/AssignmentTest.java", "cobalt_generated")
      logger.info("Default program arguments used: $args")
      start(defaultArgs)
    }

  }

  def start(args: Array[String]) {

    if (args.length == 3) {

      // If compiling a single file
      if (args(0).contains(".cobalt")) {

        SymbolTable.getInstance.rows.clear()

        val cobaltFile: File = new File(args(0))
        val asmFile: File = new File(args(1))
        val buildDir = new File(args(2))

        // Generate directories for Java ASM files
        new File(asmFile.getParent).mkdirs()

        new Runtime(cobaltFile, asmFile, buildDir).parseFile()

        val compiler = new SimpleCompiler(asmFile.getAbsolutePath)

        val loader = compiler.getClassLoader()

        val compClass = loader.loadClass(asmFile.getPath.replace(".java", "").replace("\\", ".").substring(asmFile.getPath.replace(".java", "").replace("\\", ".").indexOf(".") + 1))

        val instance = compClass.newInstance()
        compClass.getMethod("main", classOf[Array[String]]).invoke(null, Array[String]())
      }

    }
    else {
      logger.info("Error: Source, ASM, and build args required. ")
    }
  }
}
