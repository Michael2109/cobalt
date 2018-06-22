package cobalt.ast

import cobalt.ir.IR._

object AST {

  case class Module(header: ModuleHeader, models: Seq[Statement])

  def moduleToModuleIR(module: Module) = ModuleIR(moduleHeaderToModuleHeaderIR(module.header), module.models.map(statementToStatementIR))

  case class ModuleHeader(nameSpace: NameSpace, imports: Seq[Import])

  def moduleHeaderToModuleHeaderIR(moduleHeader: ModuleHeader) = ModuleHeaderIR(nameSpaceToNameSpaceIR(moduleHeader.nameSpace), moduleHeader.imports.map(importToImportIR))

  case class Import(loc: Seq[Name])

  def importToImportIR(`import`: Import) = ImportIR(`import`.loc.map(nameToNameIR))

  case class Field(name: Name, `type`: Type, init: Option[Type])

  def fieldToFieldIR(field: Field) =
  {
    if(field.init.isDefined)
    {
      FieldIR(nameToNameIR(field.name), typeToTypeIR(field.`type`), Some(typeToTypeIR(field.init.get)))
    }
    else
    {
      FieldIR(nameToNameIR(field.name), typeToTypeIR(field.`type`), None)
    }
  }

  trait Type
  case class Init() extends Type
  case class TypeRef(ref: Ref) extends Type
  case class TypeApp(ref: Ref, types: Seq[Type]) extends Type // Type application, aka Map<A,B> -> `TyApp (RefLocal "Map") [TyRef (RefLocal "A"), TyRef (RefLocal "B")]`
  //case class TypeRel(typeRel: TypeRel, `type1`: Type, `type2`: Type) extends Type // This allows things like <T extends Something> which would be `TyRel Extends (TyRef (RefLocal "T")) (TyRef (RefLocal "Something"))`

  def typeToTypeIR(`type`: Type): TypeIR = `type` match
  {
    case _: Init => InitIR()
    case typeRef: TypeRef => TypeRefIR(refToRefIR(typeRef.ref))
    case typeApp: TypeApp => TypeAppIR(refToRefIR(typeApp.ref), typeApp.types.map(typeToTypeIR))
  }

  trait Ref
  case class RefSpecial(specialRef: SpecialRef) extends Ref
  case class RefLocal(name: Name) extends Ref
  case class RefQual(qualName: QualName) extends Ref

  def refToRefIR(ref: Ref) = ref match
  {
    case refSpecial: RefSpecial => RefSpecialIR(specialRefToSpecialRefIR(refSpecial.specialRef))
    case refLocal: RefLocal => RefLocalIR(nameToNameIR(refLocal.name))
    case refQual: RefQual => RefQualIR(qualNameToQualNameIR(refQual.qualName))
  }

  trait SpecialRef
  case class Super() extends SpecialRef
  case class This() extends SpecialRef

  def specialRefToSpecialRefIR(specialRef: SpecialRef): SpecialRefIR = specialRef match
  {
    case _: Super => SuperIR()
    case _: This => ThisIR()
  }

  trait TypeRel
  case class Inherits() extends TypeRel
  case class Extends() extends TypeRel
  case class Equals() extends TypeRel

  def typeRelToTypeRelIR(typeRel: TypeRel) = typeRel match
  {
    case _: Inherits => InheritsIR
    case _: Extends => ExtendsIR
    case _: Equals => EqualsIR
  }

  case class NameSpace(nameSpace: Seq[Name])

  def nameSpaceToNameSpaceIR(nameSpace: NameSpace) = NameSpaceIR(nameSpace.nameSpace.map(nameToNameIR))

  case class Name(value: String)

  def nameToNameIR(name: Name) = NameIR(name.value)

  case class QualName(nameSpace: NameSpace, name: Name)

  def qualNameToQualNameIR(qualName: QualName) = QualNameIR(nameSpaceToNameSpaceIR(qualName.nameSpace), nameToNameIR(qualName.name))

  case class Annotation(name: Name)

  def annotationToAnnotationIR(annotation: Annotation) = AnnotationIR(nameToNameIR(annotation.name))

  trait Modifier
  case object Public extends Modifier
  case object Protected extends Modifier
  case object Private extends Modifier
  case object PackageLocal extends Modifier
  case object Abstract extends Modifier
  case object Final extends Modifier
  case object Pure extends Modifier

  def modifierToModifierIR(modifier: Modifier) = modifier match
  {
    case _: Public.type => PublicIR
    case _: Protected.type => ProtectedIR
    case _: Private.type => PrivateIR
    case _: PackageLocal.type => PackageLocalIR
    case _: Abstract.type => AbstractIR
    case _: Final.type => FinalIR
    case _: Pure.type => PureIR
  }

  trait Block extends Statement
  case class Inline(expression: Expression) extends Block
  case class DoBlock(statement: Statement) extends Block

  def blockToBlockIR(block: Block) = block match
  {
    case doBlock: DoBlock => DoBlockIR(statementToStatementIR(doBlock.statement))
    case inline: Inline => InlineIR(expressionToExpressionIR(inline.expression))
  }

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
  case class IntConst(value: Int) extends Expression
  case class DoubleConst(value: Double) extends Expression
  case class FloatConst(value: Float) extends Expression
  case class LongConst(value: Long) extends Expression
  case class Neg(expression: Expression) extends Expression

  def expressionToExpressionIR(expression: Expression): ExpressionIR = expression match
  {
    case blockExpr: BlockExpr => BlockExprIR(blockExpr.expressions.map(expressionToExpressionIR))
    case identifier: Identifier => {
      identifier.name.value match {
        case "true" => BoolConstIR(true)
        case "false" => BoolConstIR(false)
        case _ => IdentifierIR(nameToNameIR(identifier.name))
      }
    }
    case methodCall: MethodCall => {
      methodCall.name.value match {
        case "print" | "println" => MethodCallIR(nameToNameIR(methodCall.name), expressionToExpressionIR(methodCall.expression))
        case _ => MethodCallIR(nameToNameIR(methodCall.name), expressionToExpressionIR(methodCall.expression))
      }

    }
    case newClassInstance: NewClassInstance => NewClassInstanceIR(typeToTypeIR(newClassInstance.`type`), expressionToExpressionIR(newClassInstance.expression), None)
    case stringLiteral: StringLiteral => StringLiteralIR(stringLiteral.value)
    case ternary: Ternary => TernaryIR()
    case tuple: Tuple => TupleIR()
    case boolConst: BoolConst => BoolConstIR(boolConst.value)
    case not: Not => NotIR()
    case aBinary: ABinary => ABinaryIR(aBinOpToABinOpIR(aBinary.op), expressionToExpressionIR(aBinary.expression1), expressionToExpressionIR(aBinary.expression2))
    case bBinary: BBinary => BBinaryIR(bBinOpToBBinOpIR(bBinary.op), expressionToExpressionIR(bBinary.expression1), expressionToExpressionIR(bBinary.expression2))
    case rBinary: RBinary => RBinaryIR(rBinOpToRBinOpIR(rBinary.op), expressionToExpressionIR(rBinary.expression1), expressionToExpressionIR(rBinary.expression2))
    case intConst: IntConst => IntConstIR(intConst.value)
    case doubleConst: DoubleConst => DoubleConstIR(doubleConst.value)
    case floatConst: FloatConst => FloatConstIR(floatConst.value)
    case longConst: LongConst => LongConstIR(longConst.value)
    case neg: Neg => NegIR(expressionToExpressionIR(neg))
  }

  case class Array() extends Expression
  case class SpecialRefAsExpr() extends Expression

  trait Statement
  case class ClassModel(name: Name, modifiers: Seq[Modifier], fields: Seq[Field], parent: Option[Type], parentArguments: Seq[Expression], interfaces: Seq[Type], body: Statement) extends Statement
  case class ObjectModel(name: Name, modifiers: Seq[Modifier], fields: Seq[Field], parent: Option[Type], parentArguments: Seq[Expression], interfaces: Seq[Type], body: Statement) extends Statement
  case class TraitModel(name: Name, modifiers: Seq[Modifier], fields: Seq[Field], parent: Option[Type], parentArguments: Seq[Expression], interfaces: Seq[Type], body: Statement) extends Statement
  case class Method(name: Name, annotations: Seq[Annotation], fields: Seq[Field], modifiers: Seq[Modifier], returnType: Option[Type], body: Block) extends Statement
  case class For() extends Statement
  case class While() extends Statement
  case class If(condition: Expression, ifBlock: Statement, elseBlock: Option[Statement]) extends Statement
  case class Assign(name: Name, `type`: Option[Type], immutable: Boolean, block: Block) extends Statement
  case class AssignMultiple(name: Seq[Name], `type`: Option[Type], immutable: Boolean, block: Block) extends Statement
  case class Reassign(name: Name, block: Block) extends Statement
  case class Return() extends Statement
  case class Lambda() extends Statement
  case class ModelDef() extends Statement
  case class ExprAsStmt(expression: Expression) extends Statement
  case class BlockStmt(statements: Seq[Statement]) extends Statement
  case class Match() extends Statement
  case class Print() extends Statement
  case class Println() extends Statement

  def statementToStatementIR(statement: Statement): StatementIR = statement match
  {
    case assign: Assign => AssignIR(nameToNameIR(assign.name), None, assign.immutable, blockToBlockIR(assign.block))
    case blockStmt: BlockStmt => BlockStmtIR(blockStmt.statements.map(statementToStatementIR))
    case classModel: ClassModel => ClassModelIR(nameToNameIR(classModel.name), classModel.modifiers.map(modifierToModifierIR), classModel.fields.map(fieldToFieldIR), None, classModel.parentArguments.map(expressionToExpressionIR), classModel.interfaces.map(typeToTypeIR), statementToStatementIR(classModel.body))
    case objectModel: ObjectModel => ObjectModelIR(nameToNameIR(objectModel.name), objectModel.modifiers.map(modifierToModifierIR), objectModel.fields.map(fieldToFieldIR), None, objectModel.parentArguments.map(expressionToExpressionIR), objectModel.interfaces.map(typeToTypeIR), statementToStatementIR(objectModel.body))
    case traitModel: TraitModel => TraitModelIR(nameToNameIR(traitModel.name), traitModel.modifiers.map(modifierToModifierIR), traitModel.fields.map(fieldToFieldIR), None, traitModel.parentArguments.map(expressionToExpressionIR), traitModel.interfaces.map(typeToTypeIR), statementToStatementIR(traitModel.body))
    case method: Method => {
      if(method.returnType.isDefined)
      {
        MethodIR(nameToNameIR(method.name), method.annotations.map(annotationToAnnotationIR), method.fields.map(fieldToFieldIR), method.modifiers.map(modifierToModifierIR), Some(typeToTypeIR(method.returnType.get)), blockToBlockIR(method.body))
      }
      else
      {
        MethodIR(nameToNameIR(method.name), method.annotations.map(annotationToAnnotationIR), method.fields.map(fieldToFieldIR), method.modifiers.map(modifierToModifierIR), None, blockToBlockIR(method.body))
      }
    }
    case f: For => ForIR()
    case w: While => WhileIR()
    case ifStatement: If => {
      if(ifStatement.elseBlock.isDefined)
      {
        IfIR(expressionToExpressionIR(ifStatement.condition), statementToStatementIR(ifStatement.ifBlock), Some(statementToStatementIR(ifStatement.elseBlock.get)))
      }
      else
      {
        IfIR(expressionToExpressionIR(ifStatement.condition), statementToStatementIR(ifStatement.ifBlock), None)
      }

    }
    case assign: Assign =>
    {
      if(assign.`type`.isDefined)
      {
          AssignIR(nameToNameIR(assign.name), Some(typeToTypeIR(assign.`type`.get)), true, blockToBlockIR(assign.block))
      }
      else {
        AssignIR(nameToNameIR(assign.name), None, true, blockToBlockIR(assign.block))
      }
    }
    case assignMultiple: AssignMultiple => {
      if(assignMultiple.`type`.isDefined)
      {
        AssignMultipleIR(assignMultiple.name.map(nameToNameIR), Some(typeToTypeIR(assignMultiple.`type`.get)), true, blockToBlockIR(assignMultiple.block))
      }
      else
      {
        AssignMultipleIR(assignMultiple.name.map(nameToNameIR), None, true, blockToBlockIR(assignMultiple.block))
      }
    }
    case reassign: Reassign => ReassignIR(nameToNameIR(reassign.name), blockToBlockIR(reassign.block))
    case r: Return => ReturnIR()
    case _: Lambda => LambdaIR()
    case _: ModelDef => ModelDefIR()
    case exprAsStmt: ExprAsStmt => ExprAsStmtIR(expressionToExpressionIR(exprAsStmt.expression))
    case blockStmt: BlockStmt => BlockStmtIR(blockStmt.statements.map(statementToStatementIR))
    case _: Match => MatchIR()
    case _: Print => PrintIR()
    case _: Println => PrintlnIR()
    case inline: Inline => InlineIR(expressionToExpressionIR(inline.expression))
    case doBlock: DoBlock => DoBlockIR(statementToStatementIR(doBlock.statement))
  }

  case class Case(expression: Expression, block: Block)

  def caseToCaseIR(`case`: Case) = CaseIR(expressionToExpressionIR(`case`.expression), blockToBlockIR(`case`block))

  trait Operator

  trait ABinOp extends Operator
  case object Add extends ABinOp
  case object Subtract extends ABinOp
  case object Multiply extends ABinOp
  case object Divide extends ABinOp

  def aBinOpToABinOpIR(aBinOp: ABinOp) = aBinOp match
  {
    case _: Add.type => AddIR
    case _: Subtract.type => SubtractIR
    case _: Multiply.type => MultiplyIR
    case _: Divide.type => DivideIR
  }

  trait BBinOp extends Operator
  case object And extends BBinOp
  case object Or extends BBinOp

  def bBinOpToBBinOpIR(bBinOp: BBinOp) = bBinOp match
  {
    case _: And.type => AndIR
    case _: Or.type => OrIR
  }

  trait RBinOp extends Operator
  case object GreaterEqual extends RBinOp
  case object Greater extends RBinOp
  case object LessEqual extends RBinOp
  case object Less extends RBinOp
  case object Equal extends RBinOp

  def rBinOpToRBinOpIR(rBinOp: RBinOp) = rBinOp match
  {
    case _: GreaterEqual.type => GreaterEqualIR
    case _: Greater.type => GreaterIR
    case _: LessEqual.type => LessEqualIR
    case _: Less.type => LessIR
    case _: Equal.type => EqualIR
  }
}
