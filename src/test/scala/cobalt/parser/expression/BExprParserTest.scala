package cobalt.parser.expression

import cobalt.ast.AST._
import cobalt.parser.ExpressionParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class BExprParserTest extends FunSpec with Matchers
{
  describe("Boolean statement parsers")
  {
    it("Should parse boolean constant `true`")
    {
      TestUtil.parse("true", ExpressionParser.expressionParser) shouldBe Identifier(Name("true"))
    }
    it("Should parse boolean constant `false`")
    {
      TestUtil.parse("false", ExpressionParser.expressionParser) shouldBe Identifier(Name("false"))
    }
  }

  describe("Relational statement parsers")
  {
    it("Should parse `less than`")
    {
      TestUtil.parse("x > 10", ExpressionParser.expressionParser) shouldBe RBinary(Greater,Identifier(Name("x")),IntConst(10))
    }
    it("Should parse `greater than`")
    {
      TestUtil.parse("x < 10", ExpressionParser.expressionParser) shouldBe RBinary(Less,Identifier(Name("x")),IntConst(10))
    }
    it("Should parse `less than equal`")
    {
      TestUtil.parse("x >= 10", ExpressionParser.expressionParser) shouldBe RBinary(GreaterEqual,Identifier(Name("x")),IntConst(10))
    }
    it("Should parse `greater than equal`")
    {
      TestUtil.parse("x <= 10", ExpressionParser.expressionParser) shouldBe RBinary(LessEqual,Identifier(Name("x")),IntConst(10))
    }
  }
}
