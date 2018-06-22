package cobalt.parser.statement

import cobalt.ast.AST._
import cobalt.parser.StatementParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class AssignParserTest extends FunSpec with Matchers
{
  describe("Assignment parser")
  {
    it("Should parse assignment")
    {
      TestUtil.parse("let x = 10", StatementParser.statement) shouldBe Assign(Name("x"),None,true,Inline(IntConst(10)))
    }

    it("Should parse mutable assignment")
    {
      TestUtil.parse("let mutable x = 10", StatementParser.statement) shouldBe Assign(Name("x"),None,false,Inline(IntConst(10)))
    }

    it("Should parse with type defined")
    {
      TestUtil.parse("let x: Int = 10", StatementParser.statement) shouldBe Assign(Name("x"),Some(TypeRef(RefLocal(Name("Int")))),true,Inline(IntConst(10)))
    }

    it("Should parse with a do block")
    {
      val code =
        """let x = do
          |  x
          |  y
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statement) shouldBe Assign(Name("x"),None,true,DoBlock(BlockStmt(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))), ExprAsStmt(Identifier(Name("y")))))))
    }
  }

  describe("Multiple assignment parser") {
    // TODO Assign parser multiple

    // TODO "let x,y = z"

    // TODO "let mutable x,y = z"

    // TODO "let x,y: Int = z"

    // TODO "let x,y = do"
    //      "    i"
    //      "    j"

    // TODO "let x,y: Int = do"
    //      "    i"
    //      "    j"
  }
}
