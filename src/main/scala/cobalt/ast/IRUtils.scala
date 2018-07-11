package cobalt.ast

import cobalt.ast.AST._
import cobalt.ast.IRNew._
import cobalt.symbol_table.{SymbolTable, ValueEntry}

import scala.tools.asm.Opcodes

object IRUtils {

  def typeStringToTypeIR(t: String): TypeIR = {
    t match {
      case "Int" => IntType()
      case "String" => StringLiteralType()
      case "Unit" => UnitType()
    }
  }

  def typeToBytecodeType(typeIR: TypeIR): String = {
    typeIR match {
      case _: IntType => "I"
      case _: StringLiteralType => "Ljava/lang/String;"
      case _: UnitType => "V"
    }
  }

  def inferType(expression: Expression, symbolTable: SymbolTable, imports: Map[String, String]): TypeIR = {
    expression match {
      case aBinary: ABinary => inferType(aBinary.expression1, symbolTable, imports)
      case blockExpr: BlockExpr => blockExpr.expressions.map(e => inferType(e, symbolTable, imports)).head
      case identifier: Identifier => ObjectType(symbolTable.get(identifier.name.value) match {
        case v: ValueEntry => v.name
      })
      case _: IntObject => ObjectType("Ljava/lang/Object;")
      case _: IntConst => IntType()
      case newClassInstance: NewClassInstance => {
        val superClass: String = newClassInstance.`type`.ref match {
          case RefLocal(name) => imports.get(name.value).getOrElse(name.value)
          case RefQual(qualName) => qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value
        }
        ObjectType(superClass)
      }
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

  def getStoreOperator(t: TypeIR, id: Int): StoreOperators = {
    t match {
      case intType: IntType => IStore(id)
      case stringLiteralType: StringLiteralType => AStore(id);
    }
  }

  def getLoadOperator(t: TypeIR): Int = {
    t match {
      case intType: IntType => Opcodes.ILOAD
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

  def modifierToOpcode(modifier: Modifier): Int = {
    modifier match {
      case _: Public => Opcodes.ACC_PUBLIC
      case _: Protected => Opcodes.ACC_PROTECTED
      case _: Private => Opcodes.ACC_PRIVATE
      case _: Abstract => Opcodes.ACC_ABSTRACT
      case _: Final => Opcodes.ACC_FINAL
    }
  }
}
