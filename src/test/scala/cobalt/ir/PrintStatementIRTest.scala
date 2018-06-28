package cobalt.ir

import cobalt.ast.AST
import cobalt.ast.AST.{BlockExpr, Identifier, MethodCall, Name}
import cobalt.ir.IR._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class PrintStatementIRTest extends FunSpec with Matchers{

  describe("Change method calls to print/println")
  {
    it("Should change to a println method call")
    {
      AST.expressionToExpressionIR(MethodCall(Name("println"),BlockExpr(ArrayBuffer(Identifier(Name("y")))))) shouldBe MethodCallIR(178,"java/lang/System","out","Ljava/io/PrintStream;",BlockExprIR(ArrayBuffer(IdentifierIR(NameIR("y")))),182,"java/io/PrintStream","println","(I)V")
    }
  }
}
