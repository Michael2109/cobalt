package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class MethodCallCompileTest extends FunSpec with Matchers {

  describe("Method call compile test") {
    it("Should compile method calls") {

      val output: Array[String] = CompilerUtil.executeJava(Paths.get("integration/MethodCallTest"))

      val expectedResults = Array(
        "10"
      )

      for(i <- 0 until expectedResults.length){
        output(i) shouldBe expectedResults(i)
      }
    }
  }

}
