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
    it("Should parse models with no fields")
    {
      val code =
        """class Test
          |  let x = 10
          |  let exampleMethod(): Int = do
          |    1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe ClassModel(Name("Test"),List(),List(),None,List(),List(),BlockStmt(ArrayBuffer(Assign(Name("x"),None,true,Inline(IntConst(10))), Method(Name("exampleMethod"),List(),ArrayBuffer(),List(Public),Some(Type(Name("Int"))),DoBlock(BlockStmt(ArrayBuffer(ExprAsStmt(IntConst(1)))))))))
    }
  }
}
