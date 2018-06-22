package cobalt.code_gen

import cobalt.ir.IR._

import scala.tools.asm._
import scala.tools.asm.Opcodes;

object CodeGen
{

  val version = 49

  def genCode(module: ModuleIR): Array[Byte] =
  {
    val cw = new ClassWriter(0)

    module.models.foreach(x => genCode(cw, x))
    cw.visitEnd()
    cw.toByteArray
  }

  def genCode(cw: ClassWriter, statement: StatementIR): Unit =
  {
    statement match
    {
      case classModel: ClassModelIR =>
      {
        cw.visit(version, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, classModel.name.value, null, "java/lang/Object", null)
        val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
        mv.visitVarInsn(Opcodes.ALOAD, 0)
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
        mv.visitInsn(Opcodes.RETURN)
        mv.visitMaxs(1, 1)
        mv.visitEnd()
        genCode(cw, classModel.body)
      }
      case method: MethodIR =>
      {
        val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, method.name.value, "()V", null, null)
        genCode(mv, method.body)
        mv.visitInsn(Opcodes.RETURN)
        mv.visitEnd()
      }
    }
  }

  def genCode(mv: MethodVisitor, expression: ExpressionIR): Unit =
  {
    expression match
    {
      case boolConst: BoolConstIR => mv.visitIincInsn(Opcodes.BIPUSH, 1)
      case blockStmt: BlockExprIR => blockStmt.expressions.foreach(x => genCode(mv, x))
      case intConst: IntConstIR => intConstCodeGen(mv, intConst)
    }
  }

  def genCode(mv: MethodVisitor, statement: StatementIR): Unit =
  {
    statement match
    {
      case assign: AssignIR => {
        genCode(mv, assign.block)
        mv.visitVarInsn(Opcodes.ISTORE, (math.random() * 100).asInstanceOf[Int])
      }
      case blockStmt: BlockStmtIR => blockStmt.statements.foreach(x => genCode(mv, x))
      case exprAsStmt: ExprAsStmtIR => genCode(mv, exprAsStmt.expression)
      case ifStmt: IfIR => {
        val trueLabel = new Label
        val endLabel = new Label
        genCode(mv, ifStmt.condition)
        mv.visitJumpInsn(Opcodes.IFEQ, trueLabel)
        genCode(mv, ifStmt.elseBlock.getOrElse(BlockStmtIR(Seq())))
        mv.visitJumpInsn(Opcodes.GOTO, endLabel)
        mv.visitLabel(trueLabel)
        genCode(mv, ifStmt.ifBlock.asInstanceOf[BlockIR])
        mv.visitLabel(endLabel)
      }
    }
  }

  def genCode(mv: MethodVisitor, block: BlockIR): Unit =
  {
    block match
    {
      case doBlock: DoBlockIR => genCode(mv, doBlock.statement)
      case inline: InlineIR => genCode(mv, inline.expression)
    }
  }

  def intConstCodeGen(mv: MethodVisitor, intConst: IntConstIR): Unit =
  {
    mv.visitIntInsn(Opcodes.BIPUSH, intConst.value)
  }

  @throws[Exception]
  def dump: Array[Byte] =
  {
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
