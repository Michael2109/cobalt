package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil
import org.scalatest.Matchers

class NumberCompileTest extends IntegrationSetup with Matchers {

  describe("Number compile test") {
    it("Should compile numbers") {

      val output: Array[String] = CompilerUtil.executeJava(Paths.get("integration/NumberTest"))
      output(0) shouldBe "6"
      output(1) shouldBe "-1"
      output(2) shouldBe "6"
      output(3) shouldBe "10"
      output(4) shouldBe "3"
    }
  }

}
