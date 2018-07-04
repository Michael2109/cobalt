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

  def convertToIR(operator: Operator, `type`: TypeIR, method: MethodIR): Unit ={
    `type` match {
      case _: IntIR => operator match {
        case Add => method.body += IAdd
        case Subtract => method.body += ISub
        case Multiply => method.body += IMul
        case Divide => method.body += IDiv
      }
      case _: LongIR => operator match {
        case Add => method.body += LAdd
      }
      case _: ObjectIR => operator match {
        case Add =>
        case Subtract =>
        case Multiply =>
        case Divide =>
      }
    }
  }

  def convertToIR(expression: Expression, method: MethodIR): Unit ={
    expression match {
      case aBinary: ABinary => {
        convertToIR(aBinary.expression1, method)
        convertToIR(aBinary.expression2, method)
        convertToIR(aBinary.op, IRUtils.getExpressionType(aBinary.expression1),method)
      }
      case blockExpr: BlockExpr => blockExpr.expressions.foreach(e => convertToIR(e, method))
      case intCont: IntConst => method.body += ExprAsStmtIR(IntConstIR(intCont.value))
      case intObject: IntObject => {
        method.body += VisitTypeInst(Opcodes.NEW, "java/lang/Integer")
        method.body += VisitInst(Opcodes.DUP)
        convertToIR(intObject.value, method)
        method.body += VisitMethodInst(Opcodes.INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V")
      }
      case methodCall: MethodCall => {
        methodCall.name.value match {
          case "print" | "println" => {
            method.body += VisitFieldInst(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            convertToIR(boxPrimitive(methodCall.expression), method)
            method.body += VisitMethodInst(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V")
          }
        }
      }
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

  def boxPrimitive(expression: Expression): Expression ={
    IRUtils.getExpressionType(expression) match {
      case _: IntIR => IntObject(expression)
    }
  }
}

object IRNew {

  trait ModelIR

  case class ClassModelIR(nameSpace: String, name: String, methods: ListBuffer[MethodIR]) extends ModelIR
  case class MethodIR(name: String, modifiers: mutable.SortedSet[Int], fields: ListBuffer[(String, String)], body: ListBuffer[StatementIR])


  trait BlockIR extends StatementIR
  case class InlineIR(expression: ExpressionIR) extends BlockIR
  case class DoBlockIR(statement: StatementIR) extends BlockIR


  trait ExpressionIR
  case class Assign(id: Int, immutable: Boolean, block: BlockIR) extends ExpressionIR
  case class IntConstIR(value: BigInt) extends ExpressionIR

  trait StatementIR
  case class ExprAsStmtIR(expressionIR: ExpressionIR) extends StatementIR
  case class IfIR(condition: ExpressionIR, isStmt: StatementIR, elseStmt: StatementIR)
  case class LabelIR() extends StatementIR

  trait TypeIR
  case class IntIR() extends TypeIR
  case class LongIR() extends TypeIR
  case class FloatIR() extends TypeIR
  case class DoubleIR() extends TypeIR
  case class ObjectIR(name: String) extends TypeIR

  trait ArithmeticOperatorIR extends StatementIR
  case object IAdd extends ArithmeticOperatorIR
  case object ISub extends ArithmeticOperatorIR
  case object IMul extends ArithmeticOperatorIR
  case object IDiv extends ArithmeticOperatorIR
  case object LAdd extends ArithmeticOperatorIR

  case class VisitTypeInst(opcode: Int, name: String) extends StatementIR
  case class VisitInst(opcode: Int) extends StatementIR
  case class VisitFieldInst(opcode: Int, owner: String, name: String, description: String) extends StatementIR
  case class VisitMethodInst(opcode: Int, owner: String, name: String, description: String) extends StatementIR
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
