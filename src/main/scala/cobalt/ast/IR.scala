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

object IR {

  case class ModuleIR(moduleHeaderIR: ModuleHeaderIR, models: Seq[StatementIR])

  case class ModuleHeaderIR(nameSpace: NameSpaceIR, imports: Seq[ImportIR])

  case class ImportIR(loc: Seq[NameIR])

  case class FieldIR(name: NameIR, `type`: TypeIR, init: Option[TypeIR])

  trait TypeIR
  case class InitIR() extends TypeIR
  case class TypeRefIR(ref: RefIR) extends TypeIR
  case class TypeAppIR(ref: RefIR, types: Seq[TypeIR]) extends TypeIR

  trait RefIR
  case class RefSpecialIR(specialRef: SpecialRefIR) extends RefIR
  case class RefLocalIR(name: NameIR) extends RefIR
  case class RefQualIR(qualName: QualNameIR) extends RefIR

  trait SpecialRefIR
  case class SuperIR() extends SpecialRefIR
  case class ThisIR() extends SpecialRefIR

  trait TypeRelIR
  case class InheritsIR() extends TypeRelIR
  case class ExtendsIR() extends TypeRelIR
  case class EqualsIR() extends TypeRelIR

  case class NameSpaceIR(nameSpace: Seq[NameIR])

  case class NameIR(value: String)
  case class QualNameIR(nameSpace: NameSpaceIR, name: NameIR)

  case class AnnotationIR(name: NameIR)

  trait ModifierIR
  case object PublicIR extends ModifierIR
  case object ProtectedIR extends ModifierIR
  case object PrivateIR extends ModifierIR
  case object PackageLocalIR extends ModifierIR
  case object AbstractIR extends ModifierIR
  case object FinalIR extends ModifierIR
  case object PureIR extends ModifierIR
  case object StaticIR extends ModifierIR

  trait ExpressionIR
  case class BlockExprIR(expressions: Seq[ExpressionIR]) extends ExpressionIR
  case class IdentifierIR(name: NameIR) extends ExpressionIR
  case class MethodCallIR(fieldOpcode: Int, fieldOwner: String, fieldName: String, fieldDesc: String, args: ExpressionIR, methodOpcode: Int, methodOwner: String, methodName: String, methodDesc: String) extends ExpressionIR
  case class NewClassInstanceIR(`type`: TypeIR, expression: ExpressionIR, anonymousClass: Option[StatementIR]) extends ExpressionIR
  case class StringLiteralIR(value: String) extends ExpressionIR
  case class TernaryIR() extends ExpressionIR
  case class TupleIR() extends ExpressionIR
  case class BoolConstIR(value: Boolean) extends ExpressionIR
  case class NotIR() extends ExpressionIR
  case class ABinaryIR(op: ABinOpIR, expression1: ExpressionIR, expression2: ExpressionIR) extends ExpressionIR
  case class BBinaryIR(op: BBinOpIR, expression1: ExpressionIR, expression2: ExpressionIR) extends ExpressionIR
  case class RBinaryIR(op: RBinOpIR, expression1: ExpressionIR, expression2: ExpressionIR) extends ExpressionIR
  case class IntConstIR(value: BigInt) extends ExpressionIR
  case class LongConstIR(value: BigInt) extends ExpressionIR
  case class FloatConstIR(value: BigDecimal) extends ExpressionIR
  case class DoubleConstIR(value: BigDecimal) extends ExpressionIR
  case class NegIR(expression: ExpressionIR) extends ExpressionIR

  case class ArrayIR() extends ExpressionIR
  case class SpecialRefAsExprIR() extends ExpressionIR

  trait BlockIR extends StatementIR
  case class InlineIR(expression: ExpressionIR) extends BlockIR
  case class DoBlockIR(statement: StatementIR) extends BlockIR

  trait ModelIR extends StatementIR
  case class ClassModelIR(var moduleHeaderIR: ModuleHeaderIR, name: NameIR, modifiers: Seq[ModifierIR], fields: Seq[FieldIR], parent: Option[TypeIR], parentArguments: Seq[ExpressionIR], interfaces: Seq[TypeIR], body: StatementIR) extends ModelIR
  case class ObjectModelIR(var moduleHeaderIR: ModuleHeaderIR, name: NameIR, modifiers: Seq[ModifierIR], fields: Seq[FieldIR], parent: Option[TypeIR], parentArguments: Seq[ExpressionIR], interfaces: Seq[TypeIR], body: StatementIR) extends ModelIR
  case class TraitModelIR(var moduleHeaderIR: ModuleHeaderIR, name: NameIR, modifiers: Seq[ModifierIR], fields: Seq[FieldIR], parent: Option[TypeIR], parentArguments: Seq[ExpressionIR], interfaces: Seq[TypeIR], body: StatementIR) extends ModelIR

  trait StatementIR
  case class MethodIR(name: NameIR, annotations: Seq[AnnotationIR], fields: Seq[FieldIR], fieldTypes: String, modifiers: Seq[ModifierIR], returnType: Option[TypeIR], body: BlockIR) extends StatementIR
  case class ForIR() extends StatementIR
  case class WhileIR() extends StatementIR
  case class IfIR(condition: ExpressionIR, ifBlock: StatementIR, elseBlock: Option[StatementIR]) extends StatementIR
  case class AssignIR(var id: Int, `type`: Option[TypeIR], immutable: Boolean, block: BlockIR) extends StatementIR
  case class AssignMultipleIR(name: Seq[NameIR], `type`: Option[TypeIR], immutable: Boolean, block: BlockIR) extends StatementIR
  case class ReassignIR(name: NameIR, block: BlockIR) extends StatementIR
  case class ReturnIR() extends StatementIR
  case class LambdaIR() extends StatementIR
  case class ModelDefIR() extends StatementIR
  case class ExprAsStmtIR(expression: ExpressionIR) extends StatementIR
  case class BlockStmtIR(statements: Seq[StatementIR]) extends StatementIR
  case class MatchIR() extends StatementIR

  case class CaseIR(expression: ExpressionIR, block: BlockIR)

  trait OperatorIR

  trait ABinOpIR extends OperatorIR
  case object AddIR extends ABinOpIR
  case object SubtractIR extends ABinOpIR
  case object MultiplyIR extends ABinOpIR
  case object DivideIR extends ABinOpIR

  trait BBinOpIR extends OperatorIR
  case object AndIR extends BBinOpIR
  case object OrIR extends BBinOpIR

  trait RBinOpIR extends OperatorIR
  case object GreaterEqualIR extends RBinOpIR
  case object GreaterIR extends RBinOpIR
  case object LessEqualIR extends RBinOpIR
  case object LessIR extends RBinOpIR
  case object EqualIR extends RBinOpIR

}
