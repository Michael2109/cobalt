package cobalt.parser.statement

import cobalt.ast.AST._
import cobalt.parser.StatementParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class ModuleParserTest extends FunSpec with Matchers
{
  describe("ModuleIR parser")
  {
    it("Should parse modules")
    {
      val code ="package x.y.z\nclass ClassName\n  let x(): Int = 1\n"

      TestUtil.parse(code, StatementParser.moduleParser) shouldBe Module(ModuleHeader(NameSpace(ArrayBuffer(Name("x"), Name("y"), Name("z"))),ArrayBuffer()),ArrayBuffer(ClassModel(Name("ClassName"),List(),List(),None,List(),List(),Method(Name("x"),List(),ArrayBuffer(),List(),Some(TypeRef(RefLocal(Name("Int")))),Inline(IntConst(1))))))
    }
  }
}
