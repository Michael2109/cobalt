/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017 Michael Haywood
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package cobalt.ast

import cobalt.ast.IR._

import scala.tools.asm.Opcodes

object IRUtils {

  def typeToBytecodeType(value: String): String = {
    value match {
      case "Int" => "I"
      case "Long" => "L"
      case "Float" => "F"
      case "Double" => "D"
      case "String" => "Ljava/lang/String;"
    }
  }

  def getExpressionType(expression: ExpressionIR): String = {
    expression match {
      case aBinaryIR: ABinaryIR => getExpressionType(aBinaryIR.expression1)
      case blockExprIR: BlockExprIR => getExpressionType(blockExprIR.expressions.head)
      case _: IdentifierIR => "I"
      case _: IntConstIR => "I"
      case _: StringLiteralIR => "Ljava/lang/String;"
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
      case _: PublicIR.type => Opcodes.ACC_PUBLIC
      case _: PrivateIR.type => Opcodes.ACC_PRIVATE
      case _: ProtectedIR.type => Opcodes.ACC_PROTECTED
      case _: StaticIR.type => Opcodes.ACC_STATIC
    }
  }


}
