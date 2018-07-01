package cobalt.compiler

import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.file.{Path, Paths}

import cobalt.ast.{AST, IRProcessor}
import cobalt.ast.AST.Module
import cobalt.code_gen.CodeGen
import cobalt.ast.IR.{ClassModelIR, ModelIR, ModuleIR, StatementIR}
import cobalt.parser.StatementParser
import fastparse.core.Parsed

import scala.io.Source

object Compiler {

  def compile(commandLineOptions: Map[CommandLineOption, Any], classPath: Path, pathsToCompile: List[Path], outputDir: Path)= {

    val absolutePathsToCompile: List[Path] = pathsToCompile.map(path => Paths.get(classPath.toString, path.toString))

    val cobaltFiles: List[String] = absolutePathsToCompile.map(x => Source.fromFile(x.toFile).mkString)

    // Parse them
    val asts = cobaltFiles.map(cobaltFile => {
      StatementParser.moduleParser.parse(cobaltFile.replace("\r", ""))
    })

    val modules: Seq[Module] = asts.map(ast => ast match {
      case Parsed.Success(value, _) => value
      case Parsed.Failure(a, b, c) => throw new Exception("Failed compiling: " + a + " : " + b + " : " + c)
    })

    // Process AST
    val modelIRs: Seq[ModelIR] = modules.map(x => IRProcessor.restructureIR(AST.moduleToModuleIR(x))).head

    println(modelIRs)

    // Generate code
    val moduleBytecodes: Seq[Array[Byte]] = modelIRs.map(CodeGen.genModelCode)

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
