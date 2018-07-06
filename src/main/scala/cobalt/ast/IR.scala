package cobalt.ast

import cobalt.ast.AST._
import cobalt.ast.IRNew._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.tools.asm.{Label, Opcodes}

object AST2IR {

  def astToIR(module: Module): Seq[ModelIR] = {
    module.models.map(model => model match {
      case classModel: ClassModel => {

        val parent: String = classModel.parent match {
          case Some(value) => value.ref match {
            case RefLocal(name) => name.value
            case RefQual(qualName) => {
              println(qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value)
              qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value
            }
          }
          case None => "java/lang/Object"
        }

        val classModelIR = ClassModelIR(module.header.nameSpace.nameSpace.map(_.value).mkString("/"), classModel.name.value, parent, ListBuffer[String](), ListBuffer[VisitField](), ListBuffer[MethodIR]())
        convertToIR(classModel.body, classModelIR)
        addConstructor(classModelIR)
        classModelIR
      }
    })
  }

  def addConstructor(model: ClassModelIR): Unit ={
    if(!constructorExists()){
      val methodIR = MethodIR("<init>", mutable.SortedSet[Int](Opcodes.ACC_PUBLIC), ListBuffer(), "V", ListBuffer())

      methodIR.body += VisitVarInsn(Opcodes.ALOAD, 0)
      methodIR.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
      methodIR.body += VisitInsn(Opcodes.RETURN)

      model.methods += methodIR
    }
  }

  def constructorExists(): Boolean ={
    return false
  }

  def convertToIR(statement: Statement, model: ClassModelIR): Unit = {
    statement match {
      case assign: Assign => {
        model.localVariables += VisitField(model.getNextVarId(), assign.name.value, "I", null, null)
        val methodIR = MethodIR(assign.name.value, mutable.SortedSet[Int](), ListBuffer(), "V", ListBuffer())
        convertToIR(assign.block, methodIR)
        methodIR.body += VisitInsn(Opcodes.RETURN)

        model.methods += methodIR
      }
      case blockStmt: BlockStmt => blockStmt.statements.foreach(s => convertToIR(s, model))
      case method: Method => {
        val methodIR = MethodIR(method.name.value, mutable.SortedSet[Int](), ListBuffer(), "V", ListBuffer())


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
            val t = f.`type`.ref match {
              case RefLocal(name) => name.value
              case RefQual(qualName) => qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value
            }
            (methodIR.fields += ((f.name.value, IRUtils.typeToBytecodeType(t))))
          })
        }
        convertToIR(method.body, methodIR)
        methodIR.body += VisitInsn(Opcodes.RETURN)

        model.methods += methodIR
      }
    }
  }

  def convertToIR(operator: Operator, `type`: TypeIR, method: MethodIR): Unit ={
    `type` match {
      case _: IntType => operator match {
        case Add => method.body += IAdd
        case Subtract => method.body += ISub
        case Multiply => method.body += IMul
        case Divide => method.body += IDiv
      }
      case _: LongType => operator match {
        case Add => method.body += LAdd
      }
      case _: ObjectType => operator match {
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
      case identifier: Identifier => {
        identifier.name.value match {
          case "true" => convertToIR(IntConst(1), method)
          case "false" => convertToIR(IntConst(0), method)
          case _ => IdentifierIR(identifier.name.value)
        }
      }
      case intCont: IntConst => method.body += ExprAsStmtIR(IntConstIR(intCont.value))
      case intObject: IntObject => {
        method.body += VisitTypeInst(Opcodes.NEW, "java/lang/Integer")
        method.body += VisitInsn(Opcodes.DUP)
        convertToIR(intObject.value, method)
        method.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V")
      }
      case methodCall: MethodCall => {
        methodCall.name.value match {
          case "print" | "println" => {
            method.body += VisitFieldInst(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            convertToIR(boxPrimitive(methodCall.expression), method)
            method.body += VisitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V")
          }
        }
      }
      case stringLiteral: StringLiteral => {
        method.body += ExprAsStmtIR(StringLiteralIR(stringLiteral.value))
      }
    }
  }

  def convertToIR(statement: Statement, method: MethodIR): Unit ={
    statement match {
      case assign: Assign => {
        convertToIR(assign.block, method)
        method.body += IStore(method.getNextVarId())
      }
      case blockStmt: BlockStmt => blockStmt.statements.foreach(s => convertToIR(s, method))
      case inline: Inline => convertToIR(inline.expression, method)
      case doBlock: DoBlock => convertToIR(doBlock.statement, method)
      case exprAsStmt: ExprAsStmt => convertToIR(exprAsStmt.expression, method)
      case ifStmt: If => {

        convertToIR(ifStmt.condition, method)
        val trueLabel = method.createLabel()
        val endLabel = method.createLabel()

        method.body += VisitJumpInst(Opcodes.IFEQ, trueLabel)

        convertToIR(ifStmt.ifBlock, method)

        method.body += VisitJumpInst(Opcodes.GOTO, endLabel)

        method.visitLabel(trueLabel)

        if(ifStmt.elseBlock.isDefined) {
          convertToIR(ifStmt.elseBlock.get, method)
        }

        method.visitLabel(endLabel)

        /*
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
      case _: IntType => IntObject(expression)
      case _: StringLiteralType => expression
    }
  }
}

object IRNew {

  trait ModelIR

  case class ClassModelIR(nameSpace: String, name: String, parent: String, traits: ListBuffer[String], localVariables: ListBuffer[VisitField], methods: ListBuffer[MethodIR]) extends ModelIR {
    private var nextVarId = 0
    def getNextVarId(): Int ={
      val id = nextVarId
      nextVarId += 1
      return id
    }
  }

  case class MethodIR(name: String, modifiers: mutable.SortedSet[Int], fields: ListBuffer[(String, String)], returnType: String, body: ListBuffer[StatementIR]){

    private var nextVarId = 0
    def getNextVarId(): Int ={
      val id = nextVarId
      nextVarId += 1
      return id
    }

    val labels: mutable.SortedMap[Int, Label] = mutable.SortedMap[Int, Label]()
    def createLabel(): Int = {
      labels.put(labels.size, new Label)
      val id = labels.size - 1
      id
    }

    def visitLabel(id: Int): Unit ={
      body += VisitLabelIR(id)
    }
  }


  trait BlockIR extends StatementIR
  case class InlineIR(expression: ExpressionIR) extends BlockIR
  case class DoBlockIR(statement: StatementIR) extends BlockIR


  trait ExpressionIR
  case class AssignIR(id: Int, immutable: Boolean, block: BlockIR) extends ExpressionIR
  case class IdentifierIR(id: String) extends ExpressionIR
  case class IntConstIR(value: BigInt) extends ExpressionIR
  case class StringLiteralIR(value: String) extends ExpressionIR

  trait StatementIR
  case class ExprAsStmtIR(expressionIR: ExpressionIR) extends StatementIR
  case class IfIR(condition: ExpressionIR, isStmt: StatementIR, elseStmt: StatementIR)
  case class LabelIR(id: Int) extends StatementIR
  case class VisitLabelIR(id: Int) extends StatementIR

  trait TypeIR
  case class IntType() extends TypeIR
  case class LongType() extends TypeIR
  case class FloatType() extends TypeIR
  case class DoubleType() extends TypeIR
  case class StringLiteralType() extends TypeIR
  case class ObjectType(name: String) extends TypeIR

  trait ArithmeticOperatorIR extends StatementIR
  case object IAdd extends ArithmeticOperatorIR
  case object ISub extends ArithmeticOperatorIR
  case object IMul extends ArithmeticOperatorIR
  case object IDiv extends ArithmeticOperatorIR
  case object LAdd extends ArithmeticOperatorIR

  trait StoreOperators extends StatementIR
  case class IStore(id: Int) extends StoreOperators

  case class VisitField(id: Int, name: String, `type`: String, signature: String, value: Object)
  case class VisitTypeInst(opcode: Int, name: String) extends StatementIR
  case class VisitInsn(opcode: Int) extends StatementIR
  case class VisitFieldInst(opcode: Int, owner: String, name: String, description: String) extends StatementIR
  case class VisitJumpInst(opcode: Int, labelId: Int) extends StatementIR
  case class VisitMethodInsn(opcode: Int, owner: String, name: String, description: String) extends StatementIR
  case class VisitVarInsn(opcode: Int, id: Int) extends StatementIR
}
