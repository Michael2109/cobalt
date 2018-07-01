package cobalt.parser.expression

import cobalt.ast.AST.{DoubleConst, FloatConst, IntConst, LongConst}
import cobalt.ast.IR.IntConstIR
import cobalt.parser.ExpressionParser
import cobalt.utils.TestUtil
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
      TestUtil.parse("100", ExpressionParser.expressionParser) shouldBe IntConst(100)
    }

    it("Should parse longs")
    {
      TestUtil.parse("100l", ExpressionParser.expressionParser) shouldBe LongConst(100)
      TestUtil.parse("100L", ExpressionParser.expressionParser) shouldBe LongConst(100)
    }

    it("Should parse floats")
    {
      TestUtil.parse("123.123f", ExpressionParser.expressionParser) shouldBe FloatConst(123.123)
      TestUtil.parse("123.123F", ExpressionParser.expressionParser) shouldBe FloatConst(123.123)
    }

    it("Should parse doubles")
    {
      TestUtil.parse("123.123", ExpressionParser.expressionParser) shouldBe DoubleConst(123.123)
    }
  }
}
