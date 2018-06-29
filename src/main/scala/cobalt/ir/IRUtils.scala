package cobalt.ir

import cobalt.ast.AST._
import cobalt.ir.IR._

import scala.tools.asm.Opcodes

object IRUtils {

  def typeToBytecodeType(value: String): String = {
    value match {
      case "Int" => "I"
      case "Long" => "L"
      case "Float" => "F"
      case "Double" => "D"
    }
  }

  def getExpressionType(expression: ExpressionIR): String = {
    expression match {
      case aBinaryIR: ABinaryIR => getExpressionType(aBinaryIR.expression1)
      case blockExprIR: BlockExprIR => getExpressionType(blockExprIR.expressions.head)
      case _: IdentifierIR => "I"
      case _: IntConstIR => "I"
    }
  }

  def getStoreOperator(statement: StatementIR): Int = {
    statement match {
      case inline: InlineIR => getStoreOperator(inline.expression)
      case doBlock: DoBlockIR => getStoreOperator(doBlock.statement)
      case blockStmt: BlockStmtIR => getStoreOperator(blockStmt.statements.head)
    }
  }

  def getStoreOperator(expression: ExpressionIR): Int = {
    expression match {
      case aBinaryIR: ABinaryIR => getStoreOperator(aBinaryIR.expression1)
      case _: IntConstIR => Opcodes.ISTORE
      case _: LongConstIR => Opcodes.LSTORE
      case _: FloatConstIR => Opcodes.FSTORE
      case _: DoubleConstIR => Opcodes.DSTORE
    }
  }


  def getArithmeticOperator(op: OperatorIR, expression1: ExpressionIR, expression2: ExpressionIR): Int = {

    expression1 match {
      case innerABinary: ABinaryIR => {
        getArithmeticOperator(op, innerABinary.expression1, innerABinary.expression2)
      }
      case _: IntConstIR => {
        op match {
          case AddIR => Opcodes.IADD
          case SubtractIR => Opcodes.ISUB
          case MultiplyIR => Opcodes.IMUL
          case DivideIR => Opcodes.IDIV
        }
      }
      case _: LongConstIR => {
        op match {
          case AddIR => Opcodes.LADD
          case SubtractIR => Opcodes.LSUB
          case MultiplyIR => Opcodes.LMUL
          case DivideIR => Opcodes.LDIV
        }
      }
      case _: FloatConstIR => {
        op match {
          case AddIR => Opcodes.FADD
          case SubtractIR => Opcodes.FSUB
          case MultiplyIR => Opcodes.FMUL
          case DivideIR => Opcodes.FDIV
        }
      }
      case _: DoubleConstIR => {
        op match {
          case AddIR => Opcodes.DADD
          case SubtractIR => Opcodes.DSUB
          case MultiplyIR => Opcodes.DMUL
          case DivideIR => Opcodes.DDIV
        }
      }
    }
  }

  def modifierToModifierOp(modifierIR: ModifierIR): Int = {
    modifierIR match {
      case publicIR: PublicIR.type => Opcodes.ACC_PUBLIC
      case privateIR: PrivateIR.type => Opcodes.ACC_PRIVATE
      case protectedIR: ProtectedIR.type => Opcodes.ACC_PROTECTED
      case staticIR: StaticIR.type => Opcodes.ACC_STATIC
    }
  }


}
