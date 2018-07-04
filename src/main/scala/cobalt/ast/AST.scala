package cobalt.ast

import scala.tools.asm.Opcodes

object AST {

  case class Module(header: ModuleHeader, models: Seq[Model])

  case class ModuleHeader(nameSpace: NameSpace, imports: Seq[Import])

  case class Import(loc: Seq[Name])

  case class Field(name: Name, `type`: Type, init: Option[Type])

  case class Type(name: Name)

  trait Ref

  case class RefSpecial(specialRef: SpecialRef) extends Ref

  case class RefLocal(name: Name) extends Ref

  case class RefQual(qualName: QualName) extends Ref

  trait SpecialRef

  case class Super() extends SpecialRef

  case class This() extends SpecialRef

  trait TypeRel

  case class Inherits() extends TypeRel

  case class Extends() extends TypeRel

  case class Equals() extends TypeRel

  case class NameSpace(nameSpace: Seq[Name])

  case class Name(value: String)

  case class QualName(nameSpace: NameSpace, name: Name)

  case class Annotation(name: Name)

  trait Modifier
  case object Public extends Modifier
  case object Protected extends Modifier
  case object Private extends Modifier
  case object PackageLocal extends Modifier
  case object Abstract extends Modifier
  case object Final extends Modifier
  case object Pure extends Modifier

  trait Block extends Statement
  case class Inline(expression: Expression) extends Block
  case class DoBlock(statement: Statement) extends Block

  trait Expression
  case class BlockExpr(expressions: Seq[Expression]) extends Expression
  case class Identifier(name: Name) extends Expression
  case class MethodCall(name: Name, expression: Expression) extends Expression
  case class NewClassInstance(`type`: Type, expression: Expression, anonymousClass: Option[Statement]) extends Expression
  case class StringLiteral(value: String) extends Expression
  case class Ternary(condition: Expression, ifExpr: Expression, elseExpr: Expression) extends Expression
  case class Tuple() extends Expression
  case class BoolConst(value: Boolean) extends Expression
  case class Not() extends Expression
  case class ABinary(op: ABinOp, expression1: Expression, expression2: Expression) extends Expression
  case class BBinary(op: BBinOp, expression1: Expression, expression2: Expression) extends Expression
  case class RBinary(op: RBinOp, expression1: Expression, expression2: Expression) extends Expression
  case class IntConst(value: BigInt) extends Expression
  case class IntObject(value: Expression) extends Expression
  case class LongConst(value: BigInt) extends Expression
  case class DoubleConst(value: BigDecimal) extends Expression
  case class FloatConst(value: BigDecimal) extends Expression
  case class Neg(expression: Expression) extends Expression
  case class ArrayValue() extends Expression
  case class SpecialRefAsExpr() extends Expression

  trait Model extends Statement

  trait Statement
  case class ClassModel(name: Name, modifiers: Seq[Modifier], fields: Seq[Field], parent: Option[Type], parentArguments: Seq[Expression], interfaces: Seq[Type], body: Statement) extends Model
  case class ObjectModel(name: Name, modifiers: Seq[Modifier], fields: Seq[Field], parent: Option[Type], parentArguments: Seq[Expression], interfaces: Seq[Type], body: Statement) extends Model
  case class TraitModel(name: Name, modifiers: Seq[Modifier], fields: Seq[Field], parent: Option[Type], parentArguments: Seq[Expression], interfaces: Seq[Type], body: Statement) extends Model
  case class Method(name: Name, annotations: Seq[Annotation], fields: Seq[Field], modifiers: Seq[Modifier], returnType: Option[Type], body: Block) extends Statement
  case class For() extends Statement
  case class While() extends Statement
  case class If(condition: Expression, ifBlock: Statement, elseBlock: Option[Statement]) extends Statement
  case class Assign(name: Name, `type`: Option[Type], immutable: Boolean, block: Block) extends Statement
  case class AssignMultiple(name: Seq[Name], `type`: Option[Type], immutable: Boolean, block: Block) extends Statement
  case class Reassign(name: Name, block: Block) extends Statement
  case class Return() extends Statement
  case class Lambda() extends Statement
  case class ExprAsStmt(expression: Expression) extends Statement
  case class BlockStmt(statements: Seq[Statement]) extends Statement
  case class Match() extends Statement
  case class Case(expression: Expression, block: Block)

  trait Operator

  trait ABinOp extends Operator
  case object Add extends ABinOp
  case object Subtract extends ABinOp
  case object Multiply extends ABinOp
  case object Divide extends ABinOp

  trait BBinOp extends Operator
  case object And extends BBinOp
  case object Or extends BBinOp

  trait RBinOp extends Operator
  case object GreaterEqual extends RBinOp
  case object Greater extends RBinOp
  case object LessEqual extends RBinOp
  case object Less extends RBinOp
  case object Equal extends RBinOp
}
