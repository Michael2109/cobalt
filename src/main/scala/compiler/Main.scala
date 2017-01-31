package compiler

import java.io.File

object Main {

  def main(args: Array[String]) {
    if (args.length == 2) {
      val input: File = new File(args(0) + ".mlg")
      val output: File = new File(args(1) + ".java")
      new RuntimeScala(input, output)
    }
    else {
      System.out.println("Error: Input and Output file args required.")
    }
  }
}
