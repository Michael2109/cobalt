package cobalt.ir

import cobalt.ast.AST._
import cobalt.ir.IR._

import scala.tools.asm.Opcodes

object IRUtils {

  def getExpressionType(expression: Expression): String ={
    expression match {
      case _: IntConst => "I"
      case _: LongConst => "L"
      case _: FloatConst => "F"
      case _: DoubleConst => "D"
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


}
