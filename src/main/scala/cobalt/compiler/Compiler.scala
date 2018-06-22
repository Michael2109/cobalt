package cobalt.compiler

import java.io.{BufferedOutputStream, File, FileOutputStream, PrintWriter}
import java.nio.file.{Path, Paths}

import cobalt.ast.AST
import cobalt.ast.AST.Module
import cobalt.code_gen.CodeGen
import cobalt.ir.IR.ModuleIR
import cobalt.parser.StatementParser
import fastparse.core.Parsed

import scala.io.Source
import scala.tools.fusesource_embedded.jansi.AnsiConsole

class Compiler(commandLineOptions: Map[CommandLineOption, Any], classPath: Path, pathsToCompile: List[Path], outputDir: Path) {

  // Read in the files to be compiled

  def compile()= {

    val absolutePathsToCompile: List[Path] = pathsToCompile.map(path => Paths.get(classPath.toString, path.toString))

    val cobaltFiles: List[String] = absolutePathsToCompile.map(x => Source.fromFile(x.toFile).mkString)

    // Parse them
    val asts = cobaltFiles.map(cobaltFile => {
      import scala.reflect.runtime.universe._
      println(Literal(Constant(cobaltFile.replace("\r", ""))))
      StatementParser.moduleParser.parse(cobaltFile.replace("\r", ""))
    })

    val modules: List[Module] = asts.map(ast => ast match {
      case Parsed.Success(value, _) => value
      case Parsed.Failure(a, b, c) => throw new Exception("Failed compiling: " + a + " : " + b + " : " + c)
    })

    // Process AST
    val moduleIRs: List[ModuleIR] = modules.map(AST.moduleToModuleIR)

    // Generate code
    val moduleBytecodes: List[Array[Byte]] = moduleIRs.map(CodeGen.genCode)

    // Save to destination directory

    val parent = new File(outputDir.resolve(pathsToCompile(0)).getParent.toString)
    parent.mkdirs()

    // Create class file
    val file = new File(outputDir.resolve(pathsToCompile(0)).toString.replaceFirst("[.][^.]+$", "") + ".class")
    file.createNewFile()

    val bos = new BufferedOutputStream(new FileOutputStream(outputDir.resolve(pathsToCompile(0)).toString.replaceFirst("[.][^.]+$", "") + ".class"))

    bos.write(moduleBytecodes(0))
    bos.close()

  }
}
