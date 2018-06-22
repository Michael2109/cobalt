package cobalt.parser.statement

import cobalt.ast.AST._
import cobalt.parser.StatementParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class ReassignParserTest extends FunSpec with Matchers
{
  describe("Reassign parser")
  {
    it("Should parse reassignment an inline statement")
    {
      TestUtil.parse("x <- 2", StatementParser.statement) shouldBe Reassign(Name("x"),Inline(IntConst(2)))
    }

    it("Should parse reassignment with a do block")
    {
      val code =
        """x <- do
          |  1
          |  2
          |  3
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statement) shouldBe Reassign(Name("x"),DoBlock(BlockStmt(ArrayBuffer(ExprAsStmt(IntConst(1)), ExprAsStmt(IntConst(2)), ExprAsStmt(IntConst(3))))))
    }
  }
}
