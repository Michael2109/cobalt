package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil
import org.scalatest.Matchers

class StringLiteralCompileTest extends IntegrationSetup with Matchers {

  describe("String Literal compile test") {
    it("Should compile string literals") {

      val output: Array[String] = CompilerUtil.executeJava(Paths.get("integration/StringLiteralTest"))
      output(0) shouldBe "This is some text"
    }
  }

}
