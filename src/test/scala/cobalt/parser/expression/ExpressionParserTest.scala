package cobalt.parser.expression

import cobalt.ast.AST._
import cobalt.parser.ExpressionParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class ExpressionParserTest extends FunSpec with Matchers
{
  describe("Nested expression call parser test") {
    it("Should parse nested expression calls") {
      TestUtil.parse("x.toString()", ExpressionParser.expressionParser) shouldBe NestedExpr(ArrayBuffer(Identifier(Name("x")), MethodCall(Name("toString"),ArrayBuffer(BlockExpr(ArrayBuffer())))))
    }
  }

  // TODO "methodCall1().methodCall2()"

  // TODO "methodCall1(a).methodCall2(a)"

  // TODO "methodCall1(a, b, c).methodCall2(a, b, c)"

  // TODO "varName1.varName2"

  // TODO "methodCall1().varName1"

  // TODO "methodCall1(a).varName1"

  // TODO "methodCall1(a, b, c).varName1"

  // TODO "this.varName2"

  // TODO "super.varName2"
}
