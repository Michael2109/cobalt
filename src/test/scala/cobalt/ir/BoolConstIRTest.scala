package cobalt.ir

import cobalt.ast.AST
import cobalt.ast.AST.{Expression, Identifier, IntConst, Name}
import cobalt.ir.IR.BoolConstIR
import cobalt.parser.ExpressionParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AExprParserTest extends FunSpec with Matchers{

    describe("Convert identifiers to boolean constants")
    {
      it("Should convert `true`")
      {
        val expression = TestUtil.parse("true", ExpressionParser.expressionParser)
        expression shouldBe Identifier(Name("true"))
        AST.expressionToExpressionIR(expression.asInstanceOf[Expression]) shouldBe BoolConstIR(true)
      }
      it("Should convert `false`")
      {
        val expression = TestUtil.parse("false", ExpressionParser.expressionParser)
        expression shouldBe Identifier(Name("false"))
        AST.expressionToExpressionIR(expression.asInstanceOf[Expression]) shouldBe BoolConstIR(false)
      }
    }
}
