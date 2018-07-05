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

import cobalt.ast.AST._
import cobalt.ast.IRNew._

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

  def getExpressionType(expression: Expression): TypeIR = {
    expression match {
      case aBinary: ABinary => getExpressionType(aBinary.expression1)
      case blockExpr: BlockExpr => blockExpr.expressions.map(getExpressionType).head
      case _: IntObject => ObjectType("Ljava/lang/Object;")
      case _: IntConst => IntType()
      case _: StringLiteral => StringLiteralType()
    }
  }

  def getStoreOperator(statement: Statement): Int = {
    statement match {
      case inline: Inline => getStoreOperator(inline.expression)
      case doBlock: DoBlock => getStoreOperator(doBlock.statement)
      case blockStmt: BlockStmt => getStoreOperator(blockStmt.statements.head)
    }
  }

  def getStoreOperator(expression: Expression): Int = {
    expression match {
      case aBinaryIR: ABinary => getStoreOperator(aBinaryIR.expression1)
      case _: IntConstIR => Opcodes.ISTORE
      case _: LongConst => Opcodes.LSTORE
      case _: FloatConst => Opcodes.FSTORE
      case _: DoubleConst => Opcodes.DSTORE
    }
  }


  def getArithmeticOperator(op: Operator, expression1: Expression, expression2: Expression): Int = {

    expression1 match {
      case innerABinary: ABinary => {
        getArithmeticOperator(op, innerABinary.expression1, innerABinary.expression2)
      }
      case _: IntConstIR => {
        op match {
          case Add => Opcodes.IADD
          case Subtract => Opcodes.ISUB
          case Multiply => Opcodes.IMUL
          case Divide => Opcodes.IDIV
        }
      }
      case _: LongConst => {
        op match {
          case Add => Opcodes.LADD
          case Subtract => Opcodes.LSUB
          case Multiply => Opcodes.LMUL
          case Divide => Opcodes.LDIV
        }
      }
      case _: FloatConst => {
        op match {
          case Add => Opcodes.FADD
          case Subtract => Opcodes.FSUB
          case Multiply => Opcodes.FMUL
          case Divide => Opcodes.FDIV
        }
      }
      case _: DoubleConst => {
        op match {
          case Add => Opcodes.DADD
          case Subtract => Opcodes.DSUB
          case Multiply => Opcodes.DMUL
          case Divide => Opcodes.DDIV
        }
      }
    }
  }
}
