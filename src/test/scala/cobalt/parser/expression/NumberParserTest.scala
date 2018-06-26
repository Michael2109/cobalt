package cobalt.parser.expression

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class NumberParserTest extends FunSpec with Matchers
{
  describe("Number parser")
  {
    it("Should parse integers")
    {
      // TestUtil.parse("let exampleMethod (): Int = do\n if true 1 else 2", StatementParser.stmt) shouldBe Name(identifier("true"),Load)
    }

    it("Should parse longs")
    {
      // TestUtil.parse("let exampleMethod (): Int = do\n if true 1 else 2", StatementParser.stmt) shouldBe Name(identifier("true"),Load)
    }

    it("Should parse floats")
    {
      // TestUtil.parse("let exampleMethod (): Int = do\n if true 1 else 2", StatementParser.stmt) shouldBe Name(identifier("true"),Load)
    }

    it("Should parse doubles")
    {
     // TestUtil.parse("let exampleMethod (): Int = do\n if true 1 else 2", StatementParser.stmt) shouldBe Name(identifier("true"),Load)
    }
  }
}
