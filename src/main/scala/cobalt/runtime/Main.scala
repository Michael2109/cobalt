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

package cobalt.runtime

import java.io.File

import cobalt.symbol_table.SymbolTable
import org.apache.log4j.PropertyConfigurator
import org.codehaus.janino.SimpleCompiler
import org.slf4j.{Logger, LoggerFactory}


object Main {

  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]) {
    PropertyConfigurator.configure("src/log4j.properties")
    if (args.length == 4) {
      logger.info("Program arguments: $args")
      val files = Array[File](new File(args(0)), new File(args(1)), new File(args(2)), new File(args(3)))
      start(files)
    } else {
      val defaultArgs = Array(new File("cobalt_source/test/assignment/AssignmentTest.cobalt"), new File("cobalt_java/test/assignment/AssignmentTest.java"), new File("cobalt_generated/test/assignment/AssignmentTest.class"), new File("cobalt_asm"))
      logger.info("Default program arguments used: $args")
      start(defaultArgs)
    }
  }

  def start(args: Array[File]) {

    // If compiling a single file
    SymbolTable.getInstance.rows.clear()

    val cobaltFile = args(0)
    val asmFile = args(1)
    val classFile = args(2)
    val classPath = args(3)

    logger.info("Cobalt File: " + cobaltFile)

    // Generate directories for Java ASM files
    new File(asmFile.getParent).mkdirs()
    asmFile.createNewFile()

    new Runtime(cobaltFile, asmFile, classFile).parseFile()

    val compiler = new SimpleCompiler(asmFile.getAbsolutePath)

    val loader = compiler.getClassLoader()

    val compClass = loader.loadClass(asmFile.getPath.replace(".java", "").replace("\\", ".").replace((classPath.getPath + "\\")replace("\\","."),""))

    val instance = compClass.newInstance()
    compClass.getMethod("main", classOf[Array[String]]).invoke(null, Array[String]())

    logger.info("ASM File: " + asmFile + " - Class Path: " + classPath)
    logger.info("Class File: " + classFile)

  }
}
