package cobalt.parser.statement

import cobalt.ast.AST._
import cobalt.parser.StatementParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class DoBlockParserTest extends FunSpec with Matchers
{
  describe("Do block parser")
  {
    it("Should parse do block")
    {
      val code =
        """do
          |  x
          |  y
          |  z
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.doBlock) shouldBe DoBlock(BlockStmt(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))), ExprAsStmt(Identifier(Name("y"))), ExprAsStmt(Identifier(Name("z"))))))
    }
  }
}
