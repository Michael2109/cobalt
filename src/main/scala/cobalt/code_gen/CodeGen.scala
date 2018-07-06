package cobalt.code_gen

import java.io.PrintWriter

import cobalt.ast.IRNew._
import org.codehaus.janino.Java.LocalVariable

import scala.tools.asm.{Opcodes, _}
import scala.tools.asm.util.CheckClassAdapter;

object CodeGen {

  val version = 49

  def genModelCode(model: ModelIR): Array[Byte] = {
    model match {
      case classModel: ClassModelIR => {
        val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
        cw.visit(version, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, classModel.nameSpace + "/" + classModel.name, null, classModel.parent, null)
        classModel.localVariables.foreach(v => genCode(cw, v))
        classModel.methods.foreach(m => genCode(cw, m))
        cw.visitEnd()

        val pw = new PrintWriter(System.out)
        CheckClassAdapter.verify(new ClassReader(cw.toByteArray), true, pw)
        cw.toByteArray
      }
    }
  }

  def genCode(cw: ClassWriter, localVariable: VisitField): Unit ={
    cw.visitField(localVariable.id, localVariable.name, localVariable.`type`, localVariable.signature, localVariable.value)
  }

  def genCode(cw: ClassWriter, method: MethodIR): Unit = {
      val mv = cw.visitMethod(method.modifiers.foldLeft(0)(_|_), method.name, String.format("(%s)V", method.fields.map(_._2).mkString), null, null)
      method.body.foreach(x => genCode(mv, x, method))
      mv.visitMaxs(0, 0)
      mv.visitEnd()
  }

  def genCode(mv: MethodVisitor, expression: ExpressionIR, method: MethodIR): Unit = {
    expression match {
     /* case aBinary: ABinaryIR => {
        genCode(mv, aBinary.expression1)
        genCode(mv, aBinary.expression2)
        val instruction = IRUtils.getArithmeticOperator(aBinary.op, aBinary.expression1, aBinary.expression2)
        mv.visitInsn(instruction)
      }
      case boolConst: BoolConstIR => mv.visitIntInsn(Opcodes.BIPUSH,
        if (boolConst.value.equals(true)) {
          1
        } else {
          0
        })
      case blockStmt: BlockExprIR => blockStmt.expressions.foreach(x => genCode(mv, x))*/
      case intConst: IntConstIR => mv.visitIntInsn(Opcodes.BIPUSH, intConst.value.toInt)
      /*case longConst: LongConstIR => mv.visitLdcInsn(longConst.value.toLong)
      case floatConst: FloatConstIR => mv.visitLdcInsn(floatConst.value.toFloat)
      case doubleConst: DoubleConstIR => mv.visitLdcInsn(doubleConst.value.toDouble)*/
      case stringLiteral: StringLiteralIR => mv.visitLdcInsn(stringLiteral.value)
    }
  }

  def genCode(mv: MethodVisitor, statement: StatementIR, method: MethodIR): Unit = {
    statement match {
      /*case assign: AssignIR => {
        genCode(mv, assign.block)
        mv.visitVarInsn(IRUtils.getStoreOperator(assign.block), assign.id)
      }*/
      /*case blockStmt: BlockStmtIR => blockStmt.statements.foreach(x => genCode(mv, x))*/
      case doBlock: DoBlockIR => genCode(mv, doBlock.asInstanceOf[BlockIR], method)
      case exprAsStmt: ExprAsStmtIR => genCode(mv, exprAsStmt.expressionIR, method)
      /*case ifStmt: IfIR => {
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
      case IAdd => mv.visitInsn(Opcodes.IADD)
      case iStore: IStore => mv.visitVarInsn(Opcodes.ISTORE, iStore.id);
      case ISub => mv.visitInsn(Opcodes.ISUB)
      case IMul => mv.visitInsn(Opcodes.IMUL)
      case IDiv => mv.visitInsn(Opcodes.IDIV)
      case inline: InlineIR => genCode(mv, inline.asInstanceOf[BlockIR], method)
      case visitFieldInst: VisitFieldInst => mv.visitFieldInsn(visitFieldInst.opcode, visitFieldInst.owner, visitFieldInst.name, visitFieldInst.description)
      case visitJumpInsn: VisitJumpInst => mv.visitJumpInsn(visitJumpInsn.opcode, method.labels.get(visitJumpInsn.labelId).get)
      case visitMethodInst: VisitMethodInsn => mv.visitMethodInsn(visitMethodInst.opcode, visitMethodInst.owner, visitMethodInst.name, visitMethodInst.description, false)
      case visitTypeInst: VisitTypeInst => mv.visitTypeInsn(visitTypeInst.opcode, visitTypeInst.name)
      case visitInst: VisitInsn => mv.visitInsn(visitInst.opcode)
      case _: LabelIR =>
      case visitLabel: VisitLabelIR => mv.visitLabel(method.labels.get(visitLabel.id).get)
      case visitVarInsn: VisitVarInsn => mv.visitVarInsn(visitVarInsn.opcode, visitVarInsn.id)
    }
  }

  def genCode(mv: MethodVisitor, block: BlockIR, method: MethodIR): Unit = {
    block match {
      case doBlock: DoBlockIR => genCode(mv, doBlock.statement, method)
      case inline: InlineIR => genCode(mv, inline.expression, method)
    }
  }
}
