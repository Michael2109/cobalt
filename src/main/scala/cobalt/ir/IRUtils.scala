package cobalt.ir

import cobalt.ast.AST._
import cobalt.ir.IR._

import scala.tools.asm.Opcodes

object IRUtils {

  def getExpressionType(value: String): String ={
    value match {
      case "Int" => "I"
      case "Long" => "L"
      case "Float" => "F"
      case "Double" => "D"
    }
  }

  def getStoreOperator(statement: StatementIR): Int ={
    statement match {
      case inline: InlineIR => getStoreOperator(inline.expression)
      case doBlock: DoBlockIR => getStoreOperator(doBlock.statement)
      case blockStmt: BlockStmtIR => getStoreOperator(blockStmt.statements.head)
    }
  }

  def getStoreOperator(expression: ExpressionIR): Int ={
    expression match {
      case aBinaryIR: ABinaryIR => getStoreOperator(aBinaryIR.expression1)
      case _: IntConstIR => Opcodes.ISTORE
      case _: LongConstIR => Opcodes.LSTORE
      case _: FloatConstIR => Opcodes.FSTORE
      case _: DoubleConstIR => Opcodes.DSTORE
    }
  }

  def getArithmeticOperator(aBinary: ABinaryIR): Int = {
    aBinary.expression1 match {
      case innerABinary: ABinaryIR => {
        getArithmeticOperator(innerABinary)
      }
      case _: IntConstIR => {
        aBinary.op match {
          case AddIR => Opcodes.IADD
          case SubtractIR => Opcodes.ISUB
          case MultiplyIR => Opcodes.IMUL
          case DivideIR => Opcodes.IDIV
        }
      }
      case _: LongConstIR => {
        aBinary.op match {
          case AddIR => Opcodes.LADD
          case SubtractIR => Opcodes.LSUB
          case MultiplyIR => Opcodes.LMUL
          case DivideIR => Opcodes.LDIV
        }
      }
      case _: FloatConstIR => {
        aBinary.op match {
          case AddIR => Opcodes.FADD
          case SubtractIR => Opcodes.FSUB
          case MultiplyIR => Opcodes.FMUL
          case DivideIR => Opcodes.FDIV
        }
      }
      case _: DoubleConstIR => {
        aBinary.op match {
          case AddIR => Opcodes.DADD
          case SubtractIR => Opcodes.DSUB
          case MultiplyIR => Opcodes.DMUL
          case DivideIR => Opcodes.DDIV
        }
      }
    }
  }

  def modifierToModifierOp(modifierIR: ModifierIR): Int ={
    modifierIR match {
      case publicIR: PublicIR.type => Opcodes.ACC_PUBLIC
      case privateIR: PrivateIR.type => Opcodes.ACC_PRIVATE
      case protectedIR: ProtectedIR.type => Opcodes.ACC_PROTECTED
      case staticIR: StaticIR.type  => Opcodes.ACC_STATIC
    }
  }


}
