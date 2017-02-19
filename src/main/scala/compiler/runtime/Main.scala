package compiler.runtime

import java.io.File

import compiler.symbol_table.SymbolTable
import compiler.utilities.{Constants, Utils}

object Main {

  def main(args: Array[String]) {
    if (args.length == 3) {

      val fileList:Array[File] = Utils.recursiveListFiles(new File(args(0)),"cobalt".r)


      for(file <- fileList){

        SymbolTable.getInstance.rows.clear()


        println(file.getAbsolutePath)
        val input: File = file
        val asmFile: File = new File(file.getPath.replace(args(0),args(1)).replaceAll("(\\.[^\\.]*$)", ".java"))
        val buildDir = new File(args(2))
        new Runtime(input, asmFile, buildDir)
      }



    }
    else {
      System.out.println("Error: Input and Output file args required. Enter with file extension removed.")
    }
  }
}
