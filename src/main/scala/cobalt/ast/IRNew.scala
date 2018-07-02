package cobalt.ast

import cobalt.ast.AST._
import cobalt.ast.IRNew._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object AST2IR {

  def astToIR(module: Module): Seq[ModelIR] = {
    module.models.map(model => model match {
      case classModel: ClassModel => {
        val classModelIR = ClassModelIR(classModel.name.value, ListBuffer())
        convertToIR(classModel.body, classModelIR)
        classModelIR
      }
    })
  }

  def convertToIR(statement: Statement, model: ClassModelIR): Unit = {
    statement match {
      case blockStmt: BlockStmt => blockStmt.statements.foreach(s => convertToIR(s, model))
      case method: Method => {
        val methodIR = MethodIR(method.name.value, List(), ListBuffer())
        convertToIR(method.body, methodIR)
        model.methods += methodIR
      }
    }
  }

  def convertToIR(expression: Expression, method: MethodIR): Unit ={
    expression match {
      case intCont: IntConst => method.body += ExprAsStmtIR(IntConstIR(intCont.value))
    }
  }

  def convertToIR(statement: Statement, method: MethodIR): Unit ={
    statement match {
      case blockStmt: BlockStmt => blockStmt.statements.foreach(s => convertToIR(s, method))
      case inline: Inline => convertToIR(inline.expression, method)
    }
  }
}

object IRNew {

  trait ModelIR
  case class ClassModelIR(name: String, methods: ListBuffer[MethodIR]) extends  ModelIR

  case class MethodIR(name: String, fields: List[(String, String)], body: ListBuffer[StatementIR])

  case class Assign(id: Int, immutable: Boolean, block: BlockIR)

  trait BlockIR extends StatementIR
  case class InlineIR(expression: ExpressionIR) extends BlockIR
  case class DoBlockIR(statement: StatementIR) extends BlockIR

  trait ExpressionIR
  case class IntConstIR(value: BigInt) extends ExpressionIR

  trait StatementIR
  case class ExprAsStmtIR(expressionIR: ExpressionIR) extends StatementIR
}

object Test {
  def main(args: Array[String]): Unit = {
    convertAST2IR()
  }

  def convertAST2IR(): Unit = {
    val module =  Module(ModuleHeader(NameSpace(ArrayBuffer(Name("x"), Name("y"), Name("z"))),ArrayBuffer()),ArrayBuffer(ClassModel(Name("ClassName"),List(),List(),None,List(),List(),Method(Name("x"),List(),ArrayBuffer(),List(Public),Some(TypeRef(RefLocal(Name("Int")))),Inline(IntConst(1))))))
    val result = AST2IR.astToIR(module)

    println(result)
  }

}
