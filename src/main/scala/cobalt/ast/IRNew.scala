package cobalt.ast

import cobalt.ast.AST._
import cobalt.ast.IRNew._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.tools.asm.Opcodes

object AST2IR {

  def astToIR(module: Module): Seq[ModelIR] = {
    module.models.map(model => model match {
      case classModel: ClassModel => {
        val classModelIR = ClassModelIR(module.header.nameSpace.nameSpace.map(_.value).mkString("/"), classModel.name.value, ListBuffer())
        convertToIR(classModel.body, classModelIR)
        classModelIR
      }
    })
  }

  def convertToIR(statement: Statement, model: ClassModelIR): Unit = {
    statement match {
      case blockStmt: BlockStmt => blockStmt.statements.foreach(s => convertToIR(s, model))
      case method: Method => {
        val methodIR = MethodIR(method.name.value, mutable.SortedSet[Int](), ListBuffer(), ListBuffer())


        methodIR.modifiers += Opcodes.ACC_PUBLIC

        methodIR.modifiers ++ method.modifiers.map(m => {
          m match {
            case _: Public.type => Opcodes.ACC_PUBLIC
            case _: Protected.type => Opcodes.ACC_PROTECTED
            case _: Private.type => Opcodes.ACC_PRIVATE
            case _: Abstract.type => Opcodes.ACC_ABSTRACT
            case _: Final.type => Opcodes.ACC_FINAL
          }
        })

        if(methodIR.name.equals("main")){
          methodIR.modifiers += Opcodes.ACC_STATIC
          methodIR.fields += (("args", "[Ljava/lang/String;"))
        } else {
          method.fields.foreach(f => {
            (methodIR.fields += ((f.name.value, IRUtils.typeToBytecodeType(f.`type`.name.value))))
          })
        }
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
      case ifStmt: If => {
       /* val trueLabel = new LabelIR
        val endLabel = new LabelIR
        convertToIR(ifStmt.condition, method)
        method.labels += trueLabel
        method.labels += endLabel
        convertToIR(ifStmt.ifBlock, method)
        ifStmt.elseBlock match {
          case Some(elseBlock) => convertToIR(elseBlock, method)
          case _ =>
        }*/
        /*    case ifStmt: IfIR => {
        val trueLabel = new Label
        val endLabel = new Label
        genCode(mv, ifStmt.condition)
        mv.visitJumpInsn(Opcodes.IFEQ, trueLabel)
        genCode(mv, ifStmt.ifBlock)
        mv.visitJumpInsn(Opcodes.GOTO, endLabel)
        mv.visitLabel(trueLabel)
        genCode(mv, ifStmt.elseBlock.getOrElse(BlockStmtIR(Seq())))
        mv.visitLabel(endLabel)
      }*/
      }




    }
  }
}

object IRNew {

  trait ModelIR

  case class ClassModelIR(nameSpace: String, name: String, methods: ListBuffer[MethodIR]) extends ModelIR

  case class MethodIR(name: String, modifiers: mutable.SortedSet[Int], fields: ListBuffer[(String, String)], body: ListBuffer[StatementIR])

  case class Assign(id: Int, immutable: Boolean, block: BlockIR)

  trait BlockIR extends StatementIR

  case class InlineIR(expression: ExpressionIR) extends BlockIR

  case class DoBlockIR(statement: StatementIR) extends BlockIR


  trait ExpressionIR

  case class IntConstIR(value: BigInt) extends ExpressionIR

  case class MethodCallIR(fieldOpcode: Int, fieldOwner: String, fieldName: String, fieldDesc: String, args: ExpressionIR, methodOpcode: Int, methodOwner: String, methodName: String, methodDesc: String) extends ExpressionIR

  trait StatementIR

  case class ExprAsStmtIR(expressionIR: ExpressionIR) extends StatementIR

  case class IfIR(condition: ExpressionIR, isStmt: StatementIR, elseStmt: StatementIR)

  case class LabelIR() extends StatementIR
}

object Test {
  def main(args: Array[String]): Unit = {
    convertAST2IR()
  }

  def convertAST2IR(): Unit = {
    val module =  Module(ModuleHeader(NameSpace(ArrayBuffer(Name("x"), Name("y"), Name("z"))),ArrayBuffer()),ArrayBuffer(ClassModel(Name("ClassName"),List(),List(),None,List(),List(),Method(Name("x"),List(),ArrayBuffer(),List(Public),Some(Type(Name("Int"))),Inline(IntConst(1))))))
    val result = AST2IR.astToIR(module)

    println(result)
  }

}
