package cobalt.parser.expression

import cobalt.ast.AST._
import cobalt.parser.ExpressionParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class AExprParserTest extends FunSpec with Matchers
{
  describe("Number parsers")
  {
    it("Should parse integers")
    {
      TestUtil.parse("1", ExpressionParser.expressionParser) shouldBe IntConst(1)
    }
    it("Should parse negative")
    {
      TestUtil.parse("-1", ExpressionParser.expressionParser) shouldBe IntConst(-1)
    }
  }

  describe("Arithmetic parsers")
  {
    it("Should parse addition")
    {
      TestUtil.parse("1 + 2", ExpressionParser.expressionParser) shouldBe ABinary(Add,IntConst(1),IntConst(2))
    }
    it("Should parse subtract")
    {
      TestUtil.parse("1 - 2", ExpressionParser.expressionParser) shouldBe ABinary(Subtract,IntConst(1),IntConst(2))
    }
    it("Should parse multiply")
    {
      TestUtil.parse("1 * 2", ExpressionParser.expressionParser) shouldBe ABinary(Multiply,IntConst(1),IntConst(2))
    }
    it("Should parse divide")
    {
      TestUtil.parse("1 / 2", ExpressionParser.expressionParser) shouldBe ABinary(Divide,IntConst(1),IntConst(2))
    }
    it("Should parse mixed")
    {
      TestUtil.parse("1 / 100 * 3 + 200 - 4", ExpressionParser.expressionParser) shouldBe ABinary(Subtract,ABinary(Add,ABinary(Multiply,ABinary(Divide,IntConst(1),IntConst(100)),IntConst(3)),IntConst(200)),IntConst(4))
    }
    it("Should parse parentheses")
    {
      TestUtil.parse("1 / 100 * (2 + 200) - 3", ExpressionParser.expressionParser) shouldBe ABinary(Subtract,ABinary(Multiply,ABinary(Divide,IntConst(1),IntConst(100)),ABinary(Add,IntConst(2),IntConst(200))),IntConst(3))
    }
  }
}
