package cobalt.ast

import cobalt.ast.AST._
import cobalt.ast.IRNew._
import cobalt.symbol_table.{ClassEntry, MethodEntry, SymbolTable, ValueEntry}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.tools.asm.{Label, Opcodes}

object AST2IR {

  def astToIR(module: Module): Seq[ModelIR] = {
    module.models.map(model => model match {
      case classModel: ClassModel => {

        val imports: Map[String, String] = module.header.imports.map(i => i.loc.last.value -> i.loc.map(_.value).mkString("/")).toMap[String, String]

        val superClass: String = classModel.parent match {
          case Some(value) => value.ref match {
            case RefLocal(name) => imports.get(name.value).get
            case RefQual(qualName) => qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value
          }
          case None => "java/lang/Object"
        }

        val classModelIR = ClassModelIR(module.header.nameSpace.nameSpace.map(_.value).mkString("/"), classModel.name.value, superClass)

        classModelIR.externalStatements += Visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, module.header.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + classModel.name.value, null, superClass, null)

        val classSymbolTable = new ClassEntry("")
        convertToIR(classModel.body, classModelIR, classSymbolTable)
        addConstructor(classModelIR, superClass)
        classModelIR
      }
    })
  }

  def addConstructor(model: ClassModelIR, superClass: String): Unit ={
    if(!constructorExists()){
      val methodIR = MethodIR("<init>", mutable.SortedSet[Int](Opcodes.ACC_PUBLIC), ListBuffer(), "V", ListBuffer())

      methodIR.body += VisitVarInsn(Opcodes.ALOAD, 0)
      methodIR.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, superClass, "<init>", "()V")
      methodIR.body += VisitInsn(Opcodes.RETURN)

      model.methods += methodIR
    }
  }

  def constructorExists(): Boolean ={
    return false
  }

  def convertToIR(statement: Statement, model: ClassModelIR, symbolTable: SymbolTable): Unit = {
    statement match {
      case assign: Assign => {
        val name = assign.name.value
        val id = model.getNextVarId()
        model.externalStatements += VisitField(id, name, "I", null, null)
        val methodIR = MethodIR(assign.name.value, mutable.SortedSet[Int](), ListBuffer(), "V", ListBuffer())
        symbolTable.entries += new ValueEntry(name, id, IRUtils.inferType(assign.block, symbolTable))
        convertToIR(assign.block, methodIR, symbolTable)
        methodIR.body += VisitInsn(Opcodes.RETURN)

        model.methods += methodIR
      }
      case blockStmt: BlockStmt => blockStmt.statements.foreach(s => convertToIR(s, model, symbolTable))
      case method: Method => {
        val methodIR = MethodIR(method.name.value, mutable.SortedSet[Int](), ListBuffer(), "V", ListBuffer())

        methodIR.modifiers += Opcodes.ACC_PUBLIC

        methodIR.modifiers ++ method.modifiers.map(IRUtils.modifierToOpcode)

        if(methodIR.name.equals("main")){
          methodIR.modifiers += Opcodes.ACC_STATIC
          methodIR.fields += (("args", "[Ljava/lang/String;"))
        } else {
          method.fields.foreach(f => {
            val t = f.`type`.ref match {
              case RefLocal(name) => name.value
              case RefQual(qualName) => qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value
            }
            (methodIR.fields += ((f.name.value, IRUtils.typeToBytecodeType(IRUtils.typeStringToTypeIR(t)))))
          })
        }
        val methodSymbolTable: SymbolTable = new MethodEntry(methodIR.name, "")

        convertToIR(method.body, methodIR, methodSymbolTable)
        methodIR.body += VisitInsn(Opcodes.RETURN)

        model.methods += methodIR
      }
    }
  }

  def convertToIR(operator: Operator, `type`: TypeIR, method: MethodIR, symbolTable: SymbolTable): Unit ={
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

  def convertToIR(expression: Expression, method: MethodIR, symbolTable: SymbolTable): Unit ={
    expression match {
      case aBinary: ABinary => {
        convertToIR(aBinary.expression1, method, symbolTable)
        convertToIR(aBinary.expression2, method, symbolTable)
        convertToIR(aBinary.op, IRUtils.inferType(aBinary.expression1, symbolTable),method, symbolTable)
      }
      case blockExpr: BlockExpr => blockExpr.expressions.foreach(e => convertToIR(e, method, symbolTable))
      case identifier: Identifier => {
        val id = identifier.name.value match {
          case "true" => convertToIR(IntConst(1), method, symbolTable)
          case "false" => convertToIR(IntConst(0), method, symbolTable)
          case _ => {
            method.body += VisitTypeInst(Opcodes.NEW, "java/lang/Integer")
            method.body += VisitInsn(Opcodes.DUP)
            method.body += ExprAsStmtIR(IdentifierIR(symbolTable.get(identifier.name.value) match {
              case v: ValueEntry => v.id
            }, IntType()))
            method.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V")
          }
        }
      }
      case intCont: IntConst => method.body += ExprAsStmtIR(IntConstIR(intCont.value))
      case intObject: IntObject => {
        method.body += VisitTypeInst(Opcodes.NEW, "java/lang/Integer")
        method.body += VisitInsn(Opcodes.DUP)
        convertToIR(intObject.value, method, symbolTable)
        method.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V")
      }
      case methodCall: MethodCall => {
        methodCall.name.value match {
          case "print" | "println" => {
            method.body += VisitFieldInst(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            convertToIR(boxPrimitive(methodCall.expression, symbolTable), method, symbolTable)
            method.body += VisitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V")
          }
        }
      }
      case stringLiteral: StringLiteral => {
        method.body += ExprAsStmtIR(StringLiteralIR(stringLiteral.value))
      }
    }
  }

  def convertToIR(statement: Statement, method: MethodIR, symbolTable: SymbolTable): Unit ={
    statement match {
      case assign: Assign => {
        val id = method.getNextVarId()
        symbolTable.entries += new ValueEntry(assign.name.value, id, IRUtils.inferType(statement, symbolTable))
        convertToIR(assign.block, method, symbolTable)
        method.body += IStore(id)
      }
      case blockStmt: BlockStmt => blockStmt.statements.foreach(s => convertToIR(s, method, symbolTable))
      case inline: Inline => convertToIR(inline.expression, method, symbolTable)
      case doBlock: DoBlock => convertToIR(doBlock.statement, method, symbolTable)
      case exprAsStmt: ExprAsStmt => convertToIR(exprAsStmt.expression, method, symbolTable)
      case ifStmt: If => {

        convertToIR(ifStmt.condition, method, symbolTable)
        val trueLabel = method.createLabel()
        val endLabel = method.createLabel()

        method.body += VisitJumpInst(Opcodes.IFEQ, trueLabel)

        convertToIR(ifStmt.ifBlock, method, symbolTable)

        method.body += VisitJumpInst(Opcodes.GOTO, endLabel)

        method.visitLabel(trueLabel)

        if(ifStmt.elseBlock.isDefined) {
          convertToIR(ifStmt.elseBlock.get, method, symbolTable)
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

  def boxPrimitive(expression: Expression, symbolTable: SymbolTable): Expression ={
    IRUtils.inferType(expression, symbolTable) match {
      case _: IntType => IntObject(expression)
      case _: StringLiteralType => expression
      case _: ObjectType => expression
    }
  }
}

object IRNew {

  trait ModelIR

  case class ClassModelIR(nameSpace: String, name: String, parent: String) extends ModelIR {

    val traits: ListBuffer[String] = ListBuffer[String]()
    val externalStatements: ListBuffer[StatementIR] = ListBuffer[StatementIR]()
    val methods: ListBuffer[MethodIR] = ListBuffer[MethodIR]()
    val imports: mutable.SortedMap[String, String] = mutable.SortedMap[String, String]()

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
  case class IdentifierIR(id: Int, `type`: TypeIR) extends ExpressionIR
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

  case class Visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) extends StatementIR
  case class VisitField(id: Int, name: String, `type`: String, signature: String, value: Object) extends StatementIR
  case class VisitTypeInst(opcode: Int, name: String) extends StatementIR
  case class VisitInsn(opcode: Int) extends StatementIR
  case class VisitFieldInst(opcode: Int, owner: String, name: String, description: String) extends StatementIR
  case class VisitJumpInst(opcode: Int, labelId: Int) extends StatementIR
  case class VisitMethodInsn(opcode: Int, owner: String, name: String, description: String) extends StatementIR
  case class VisitVarInsn(opcode: Int, id: Int) extends StatementIR
}
