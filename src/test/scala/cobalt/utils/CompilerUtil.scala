package cobalt.utils

import java.io.File
import java.nio.file.Path

import cobalt.compiler.CompilerExecutor

object CompilerUtil {

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def compileDirectory(classPath: Path, outputDir: Path, currentDir: Path): Unit = {
    val allFiles = recursiveListFiles(classPath.resolve(currentDir).toFile).filter(!_.isDirectory)

    for(file <- allFiles){
      CompilerExecutor.main(Array("-cp", classPath.toString, "-d", outputDir.toString, classPath.relativize(file.toPath).toString));
    }
  }
}
