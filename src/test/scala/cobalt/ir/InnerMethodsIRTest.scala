package cobalt.ir

import cobalt.ast.AST
import cobalt.ast.AST.{Method, _}
import cobalt.ast.IR.BoolConstIR
import cobalt.parser.{ExpressionParser, StatementParser}
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class InnerMethodsIRTest extends FunSpec with Matchers{
/*
  describe("Move inner methods")
  {
    it("Should move an inner method to the model level")
    {
      val code =
        """class Test
          |  let outerMethod() = do
          |    let innerMethod() = do
          |      1
        """.stripMargin.replace("\r", "")
      val classStatement = TestUtil.parse(code, StatementParser.statementParser)
      classStatement shouldBe
        ClassModel(Name("Test"),List(),List(),None,List(),List(),
          Method(Name("outerMethod"),List(),ArrayBuffer(),List(),None,
            DoBlock(
              BlockStmt(
                ArrayBuffer(
                  Method(Name("innerMethod"),List(),ArrayBuffer(),List(),None,
                    DoBlock(
                      BlockStmt(
                        ArrayBuffer(
                          ExprAsStmt(
                            IntConst(1)))))))))))
      AST.statementToStatementIR(classStatement.asInstanceOf[Statement]) shouldBe
        ClassModel(Name("Test"),List(),List(),None,List(),List(),
          BlockStmt(Seq(
            Method(Name("outerMethod"),List(),ArrayBuffer(),List(),None,
              DoBlock(
                BlockStmt(
                  ArrayBuffer()))),
            Method(Name("innerMethod"),List(),ArrayBuffer(),List(),None,
              DoBlock(
                BlockStmt(
                  ArrayBuffer(
                    ExprAsStmt(
                      IntConst(1))))))))
        )
    }
  }*/
}
