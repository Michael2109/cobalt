package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil.compileDirectory
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSpec, FunSuite}

/**
  * Compiles all files in the resources folder and stores them in cobalt_generated
  */

@RunWith(classOf[JUnitRunner])
class IntegrationSetup extends FunSpec with BeforeAndAfterAll {

  override def beforeAll() {
    compileDirectory(Paths.get("src\\test\\resources\\cobalt"), Paths.get("cobalt_generated"), Paths.get(""))
  }
}
