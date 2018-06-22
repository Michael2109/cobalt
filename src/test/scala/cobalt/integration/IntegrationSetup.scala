package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil.compileDirectory
import org.scalatest.{BeforeAndAfterAll, FunSpec, FunSuite}

/**
  * Compiles all files in the resources folder and stores them in cobalt_generated
  */
class IntegrationSetup extends FunSpec with BeforeAndAfterAll {

  override def beforeAll() {
    println("Running before all")
    compileDirectory(Paths.get("src\\test\\resources\\cobalt"), Paths.get("cobalt_generated"), Paths.get(""))
  }
}
