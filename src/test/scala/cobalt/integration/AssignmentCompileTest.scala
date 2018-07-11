package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class AssignmentCompileTest extends FunSpec with Matchers {

  describe("Assignment compile test") {
    it("Should compile different assignments") {

      val output: Array[String] = CompilerUtil.executeJava(Paths.get("integration/AssignmentTest"))
      output(0) shouldBe "10"
    }
  }

}
