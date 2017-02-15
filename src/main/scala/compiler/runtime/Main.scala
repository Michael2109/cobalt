package compiler.runtime

import java.io.File
import java.lang.Runtime

object Main {

  def main(args: Array[String]) {
    if (args.length == 2) {
      val input: File = new File(args(0) + ".mlg")
      val asmFile: File = new File(args(1) + ".java")
     //val generatedFile: File = new File(args(2) + ".class")
      new Runtime(input, asmFile)
    }
    else {
      System.out.println("Error: Input and Output file args required. Enter with file extension removed.")
    }
  }
}
