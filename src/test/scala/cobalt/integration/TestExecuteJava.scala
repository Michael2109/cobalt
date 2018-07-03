package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil
import org.scalatest.Matchers

class TestExecuteJava extends IntegrationSetup with Matchers {

  describe("Execute a Java class") {
    it("Should execute a Java class") {
      CompilerUtil.executeJava(Paths.get("test/Test"))
    }
  }

}
