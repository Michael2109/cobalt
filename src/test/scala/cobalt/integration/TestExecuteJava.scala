package cobalt.integration

import java.io.File

import cobalt.utils.CompilerUtil
import org.scalatest.Matchers

class TestExecuteJava extends IntegrationSetup with Matchers {

  describe("Execute a Java class") {
    it("Should execute a Java class") {
      CompilerUtil.executeJava("cobalt_generated", "test.Test")
    }
  }

}
