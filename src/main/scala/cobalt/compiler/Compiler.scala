package cobalt.compiler

import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.file.{Path, Paths}

import cobalt.ast.AST2IR
import cobalt.ast.AST.Module
import cobalt.ast.IRNew.ModelIR
import cobalt.code_gen.{CodeGen, CodeGenNew}
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
    val modelIRs: Seq[ModelIR] = modules.map(x => AST2IR.astToIR(x)).head

    println(modelIRs)

    // Generate code
    val moduleBytecodes: Seq[Array[Byte]] = modelIRs.map(CodeGenNew.genModelCode)

    // Save to destination directory


    val generatedFilePath = outputDir.resolve(pathsToCompile(0))

    generatedFilePath.getParent.toFile.mkdirs()

    val filePath = generatedFilePath.toString.replaceFirst("[.][^.]+$", "") + ".class"

    println(filePath)

    // Create class file
    val file = new File(filePath)
    file.createNewFile()

    val bos = new BufferedOutputStream(new FileOutputStream(filePath))

    bos.write(moduleBytecodes(0))
    bos.close()

  }
}
