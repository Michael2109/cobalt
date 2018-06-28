package cobalt.parser.expression

import cobalt.ast.AST.{BlockExpr, Identifier, MethodCall, Name}
import cobalt.parser.ExpressionParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class MethodCallParserTest extends FunSpec with Matchers
{

  describe("Method call parser test")
  {
    it("Should parse method calls - No arguments")
    {
      TestUtil.parse("println()", ExpressionParser.expressionParser) shouldBe MethodCall(Name("println"),BlockExpr(ArrayBuffer()))
    }
    it("Should parse method calls - Single argument")
    {
      TestUtil.parse("methodCall(a)", ExpressionParser.expressionParser) shouldBe MethodCall(Name("methodCall"),BlockExpr(ArrayBuffer(Identifier(Name("a")))))
    }
    it("Should parse method calls - Multiple arguments")
    {
      TestUtil.parse("methodCall(a, b, c)", ExpressionParser.expressionParser) shouldBe MethodCall(Name("methodCall"),BlockExpr(ArrayBuffer(Identifier(Name("a")), Identifier(Name("b")), Identifier(Name("c")))))
    }
  }
}
