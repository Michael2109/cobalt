package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class NumberCompileTest extends FunSpec with Matchers {

  describe("Number compile test") {
    it("Should compile numbers") {

      val output: Array[String] = CompilerUtil.executeJava(Paths.get("integration/NumberTest"))

      val expectedResults = Array(
        "6",
        "6",
        "6.6000004",
        "6.6",
        "-1",
        "-1",
        "-1.1",
        "-1.1",
        "6",
        "6",
        "7.986",
        "7.986000000000001",
        "10",
        "10",
        "9.910891",
        "9.91089108910891",
        "3",
        "3",
        "3.1200004",
        "3.1200000000000006"
      )

      for(i <- 0 until expectedResults.length){
        output(i) shouldBe expectedResults(i)
      }
    }
  }

}
