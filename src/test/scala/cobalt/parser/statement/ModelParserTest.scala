package cobalt.parser.statement

import cobalt.ast.AST._
import cobalt.parser.StatementParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class ModelParserTest extends FunSpec with Matchers
{
  describe("Model parser")
  {
    it("Should parse a model with no fields")
    {
      val code =
        """class Test
          |  let x = 10
          |  let exampleMethod(): Int = do
          |    1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe ClassModel(Name("Test"),List(),List(),None,List(),ArrayBuffer(),BlockStmt(ArrayBuffer(Assign(Name("x"),None,true,Inline(IntConst(10))), Method(Name("exampleMethod"),List(),ArrayBuffer(),ArrayBuffer(),Some(Type(RefLocal(Name("Int")))),DoBlock(BlockStmt(ArrayBuffer(ExprAsStmt(IntConst(1)))))))))
    }

    it("Should parse a model that extends a parent")
    {
      val code =
        """class Test extends ParentClass
          |  let exampleMethod(): Int = 1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe ClassModel(Name("Test"),List(),List(),Some(Type(RefLocal(Name("ParentClass")))),List(),ArrayBuffer(),BlockStmt(ArrayBuffer(Method(Name("exampleMethod"),List(),ArrayBuffer(),ArrayBuffer(),Some(Type(RefLocal(Name("Int")))),Inline(IntConst(1))))))
    }

    it("Should parse a model that extends a parent and implements a trait")
    {
      val code =
        """class Test extends ParentClass with Trait
          |  let exampleMethod(): Int = 1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe ClassModel(Name("Test"),List(),List(),Some(Type(RefLocal(Name("ParentClass")))),List(),ArrayBuffer(Type(RefLocal(Name("Trait")))),BlockStmt(ArrayBuffer(Method(Name("exampleMethod"),List(),ArrayBuffer(),ArrayBuffer(),Some(Type(RefLocal(Name("Int")))),Inline(IntConst(1))))))
    }

    it("Should parse a model that extends a parent and implements multiple traits")
    {
      val code =
        """class Test extends ParentClass with Trait1 with Trait2 with Trait3
          |  let exampleMethod(): Int = 1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe ClassModel(Name("Test"),List(),List(),Some(Type(RefLocal(Name("ParentClass")))),List(),ArrayBuffer(Type(RefLocal(Name("Trait1"))), Type(RefLocal(Name("Trait2"))), Type(RefLocal(Name("Trait3")))),BlockStmt(ArrayBuffer(Method(Name("exampleMethod"),List(),ArrayBuffer(),ArrayBuffer(),Some(Type(RefLocal(Name("Int")))),Inline(IntConst(1))))))
    }
  }
}
