package cobalt.ast

import cobalt.ast.AST._
import cobalt.ast.IRNew._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object AST2IR {

  def astToIR(module: Module): Seq[ModelIR] = {
    module.models.map(model => model match {
      case classModel: ClassModel => {
        val classModelIR = ClassModelIR(module.header.nameSpace.nameSpace.mkString("/"), classModel.name.value, ListBuffer())
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
      case blockExpr: BlockExpr => blockExpr.expressions.foreach(e => convertToIR(e, method))
      case intCont: IntConst => method.body += ExprAsStmtIR(IntConstIR(intCont.value))
      case methodCall: MethodCall => convertToIR(methodCall.expression, method)
    }
  }

  def convertToIR(statement: Statement, method: MethodIR): Unit ={
    statement match {
      case blockStmt: BlockStmt => blockStmt.statements.foreach(s => convertToIR(s, method))
      case inline: Inline => convertToIR(inline.expression, method)
      case doBlock: DoBlock => convertToIR(doBlock.statement, method)
      case exprAsStmt: ExprAsStmt => convertToIR(exprAsStmt.expression, method)
    }
  }
}

object IRNew {

  trait ModelIR
  case class ClassModelIR(nameSpace: String, name: String, methods: ListBuffer[MethodIR]) extends  ModelIR

  case class MethodIR(name: String, fields: List[(String, String)], body: ListBuffer[StatementIR])

  case class Assign(id: Int, immutable: Boolean, block: BlockIR)

  trait BlockIR extends StatementIR
  case class InlineIR(expression: ExpressionIR) extends BlockIR
  case class DoBlockIR(statement: StatementIR) extends BlockIR

  trait ExpressionIR
  case class IntConstIR(value: BigInt) extends ExpressionIR
  case class MethodCallIR(fieldOpcode: Int, fieldOwner: String, fieldName: String, fieldDesc: String, args: ExpressionIR, methodOpcode: Int, methodOwner: String, methodName: String, methodDesc: String) extends ExpressionIR

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
