package compiler.runtime

import java.io.File

import compiler.{Constants, Utils}
import compiler.symbol_table.SymbolTable

import scala.util.matching.Regex

object Main {

  def main(args: Array[String]) {
    if (args.length == 3) {

      val fileList:Array[File] = Utils.recursiveListFiles(new File(args(0)),"cobalt".r)

      Constants.BUILD_DIR = args(2)

      for(file <- fileList){

        SymbolTable.getInstance.rows.clear()


        println(file.getAbsolutePath)
        val input: File = file
        val asmFile: File = new File(file.getPath.replace(args(0),args(1)).replaceAll("(\\.[^\\.]*$)", ".java"))
        new Runtime(input, asmFile)
      }



    }
    else {
      System.out.println("Error: Input and Output file args required. Enter with file extension removed.")
    }
  }
}
