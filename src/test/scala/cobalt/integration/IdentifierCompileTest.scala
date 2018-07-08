package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil
import org.scalatest.{FunSpec, Matchers}

class IdentifierCompileTest extends FunSpec with Matchers {

  describe("Identifier compile test") {
    it("Should compile identifiers") {

      val output: Array[String] = CompilerUtil.executeJava(Paths.get("integration/IdentifierTest"))
      output(0) shouldBe "10"
    }
  }

}
