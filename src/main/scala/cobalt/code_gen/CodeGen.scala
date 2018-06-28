package cobalt.code_gen

import java.io.PrintWriter

import cobalt.ir.IR._
import cobalt.ir.IRUtils

import scala.tools.asm._
import scala.tools.asm.Opcodes
import scala.tools.asm.util.CheckClassAdapter;

object CodeGen {

  val version = 49

  def genModelCode(model: StatementIR): Array[Byte] = {
    model match {
      case classModel: ClassModelIR => {
        val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
        cw.visit(version, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, classModel.moduleHeaderIR.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + classModel.name.value, null, "java/lang/Object", null)
        val constructor = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
        constructor.visitVarInsn(Opcodes.ALOAD, 0)
        constructor.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
        constructor.visitInsn(Opcodes.RETURN)
        constructor.visitMaxs(0, 0)
        constructor.visitEnd()
        genCode(cw, classModel.body)
        cw.visitEnd()

        val pw = new PrintWriter(System.out)
        CheckClassAdapter.verify(new ClassReader(cw.toByteArray), true, pw)

        cw.toByteArray


      }
    }
  }

  def genCode(cw: ClassWriter, statement: StatementIR): Unit = {
    statement match {
      case method: MethodIR => {
        val mv = cw.visitMethod(method.modifiers.map(IRUtils.modifierToModifierOp).foldLeft(0)(_+_), method.name.value, String.format("(%s)V", method.fieldTypes), null, null)
        genCode(mv, method.body)
        mv.visitInsn(Opcodes.RETURN)
        mv.visitMaxs(0, 0)
        mv.visitEnd()
      }
    }
  }

  def genCode(mv: MethodVisitor, expression: ExpressionIR): Unit = {
    expression match {
      case aBinary: ABinaryIR => {
        genCode(mv, aBinary.expression1)
        genCode(mv, aBinary.expression2)
        val instruction = IRUtils.getArithmeticOperator(aBinary)
        mv.visitInsn(instruction)
      }
      case boolConst: BoolConstIR => mv.visitIntInsn(Opcodes.BIPUSH,
        if (boolConst.value.equals(true)) {
          1
        } else {
          0
        })
      case blockStmt: BlockExprIR => blockStmt.expressions.foreach(x => genCode(mv, x))
      case intConst: IntConstIR => mv.visitIntInsn(Opcodes.BIPUSH, intConst.value.toInt)
      case longConst: LongConstIR => mv.visitLdcInsn(longConst.value.toLong)
      case floatConst: FloatConstIR => mv.visitLdcInsn(floatConst.value.toFloat)
      case doubleConst: DoubleConstIR => mv.visitLdcInsn(doubleConst.value.toDouble)
      case methodCall: MethodCallIR => {
        mv.visitFieldInsn(methodCall.fieldOpcode, methodCall.fieldOwner, methodCall.fieldName, methodCall.fieldDesc)
        genCode(mv, methodCall.args)
        mv.visitMethodInsn(methodCall.methodOpcode, methodCall.methodOwner, methodCall.methodName, methodCall.methodDesc)
      }
    }
  }

  def genCode(mv: MethodVisitor, statement: StatementIR): Unit = {
    statement match {
      case assign: AssignIR => {
        genCode(mv, assign.block)
        mv.visitVarInsn(IRUtils.getStoreOperator(assign.block), (math.random() * 100).asInstanceOf[Int])
      }
      case blockStmt: BlockStmtIR => blockStmt.statements.foreach(x => genCode(mv, x))
      case doBlock: DoBlockIR => genCode(mv, doBlock.asInstanceOf[BlockIR])
      case exprAsStmt: ExprAsStmtIR => genCode(mv, exprAsStmt.expression)
      case ifStmt: IfIR => {
        val trueLabel = new Label
        val endLabel = new Label
        genCode(mv, ifStmt.condition)
        mv.visitJumpInsn(Opcodes.IFEQ, trueLabel)
        genCode(mv, ifStmt.ifBlock)
        mv.visitJumpInsn(Opcodes.GOTO, endLabel)
        mv.visitLabel(trueLabel)
        genCode(mv, ifStmt.elseBlock.getOrElse(BlockStmtIR(Seq())))
        mv.visitLabel(endLabel)
      }
      case inline: InlineIR => genCode(mv, inline.asInstanceOf[BlockIR])
    }
  }

  def genCode(mv: MethodVisitor, block: BlockIR): Unit = {
    block match {
      case doBlock: DoBlockIR => genCode(mv, doBlock.statement)
      case inline: InlineIR => genCode(mv, inline.expression)
    }
  }

  @throws[Exception]
  def dump: Array[Byte] = {
    val cw = new ClassWriter(0)
    var mv: MethodVisitor = null

    cw.visit(49, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, "Hello", null, "java/lang/Object", null)
    cw.visitSource("Hello.java", null)
    mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
    mv.visitVarInsn(Opcodes.ALOAD, 0)
    mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
    mv.visitInsn(Opcodes.RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()

    mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    mv.visitLdcInsn("hello")
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V")
    mv.visitInsn(Opcodes.RETURN)
    mv.visitMaxs(2, 1)
    mv.visitEnd()

    cw.visitEnd()
    cw.toByteArray
  }
}
