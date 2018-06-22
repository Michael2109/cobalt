package cobalt.parser.statement

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class ModelParserTest extends FunSpec with Matchers
{
  describe("Model parser")
  {
    it("Should parse models with no fields")
    {
     // TestUtil.parse("let exampleMethod (): Int = do\n if true 1 else 2", StatementParser.stmt) shouldBe Name(identifier("true"),Load)
    }
  }
}
