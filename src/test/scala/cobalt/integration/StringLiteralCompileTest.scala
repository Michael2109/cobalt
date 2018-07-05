package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil
import org.scalatest.{FunSpec, Matchers}

class StringLiteralCompileTest extends FunSpec with Matchers {

  describe("String Literal compile test") {
    it("Should compile string literals") {

      val output: Array[String] = CompilerUtil.executeJava(Paths.get("integration/StringLiteralTest"))
      output(0) shouldBe "This is some text"
    }
  }

}
