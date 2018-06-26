package cobalt.parser.statement

import cobalt.ast.AST.{Annotation, Name}
import cobalt.parser.ExpressionParser
import cobalt.utils.TestUtil
import com.sun.xml.internal.ws.wsdl.parser.ParserUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class AnnotationParserTest extends FunSpec with Matchers
{

  describe("Annotation parser"){

    it("Annotation lower case"){
      TestUtil.parse("@annotation", ExpressionParser.annotationParser) shouldBe Annotation(Name("annotation"))
    }

    it("Annotation upper case"){
      TestUtil.parse("@ANNOTATION", ExpressionParser.annotationParser) shouldBe Annotation(Name("ANNOTATION"))
    }

    it("Annotation mixed"){
      TestUtil.parse("@Annotation", ExpressionParser.annotationParser) shouldBe Annotation(Name("Annotation"))
    }

  }

}
