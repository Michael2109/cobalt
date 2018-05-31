module AST.IR where

import Data.Scientific

import qualified JVM.ClassFile

data ModuleIR = ModuleIR ModuleHeaderIR [ModelIR]
    deriving (Show)

data ModuleHeaderIR = ModuleHeaderIR
    { modNameSpace :: NameSpaceIR
    , modImports :: [ImportIR]
    }
    deriving (Show, Eq)

data MethodIR = MethodIR
    { methodName :: NameIR
    , methodAnns :: [AnnotationIR]
    , methodParams :: [FieldIR]
    , methodModifiers :: [ModifierIR]
    , methodReturnType :: (Maybe TypeIR)
    , methodBody :: BlockIR
    }
    deriving (Show, Eq)

data ConstantIR = ConstantIR
    deriving (Show, Eq)

data ModelTypeIR
    = ClassModelIR
    | ObjectModelIR
    | TraitModelIR
    | UnknownModelIR
    deriving (Eq, Show)

data ModifierIR
    = PublicIR
    | ProtectedIR
    | PrivateIR
    | PackageLocalIR
    | AbstractIR
    | FinalIR
    | PureIR
    deriving (Eq, Show)

data ModelIR = ModelIR
    { modelName :: NameIR
    , modelType :: ModelTypeIR
    , modelModifiers :: [ModifierIR]
    , modelFields :: [FieldIR]
    , modelParent :: Maybe TypeIR
    , modelParentArguments :: [StmtIR]
    , modelInterfaces :: [TypeIR]
    , modelBody :: StmtIR
    }
    deriving (Show, Eq)

data FieldIR = FieldIR
    { fieldName :: NameIR
    , fieldType :: Maybe TypeIR
    , fieldInit :: Maybe ExprIR
    }
    deriving (Show, Eq)

data TypeIR
    = InitIR
    | TypeRefIR RefIR
    | TypeAppIR RefIR [TypeIR] -- type application, aka Map<A,B> -> `TyApp (RefLocal "Map") [TyRef (RefLocal "A"), TyRef (RefLocal "B")]`
    | TypeRelIR TypeRelIR TypeIR TypeIR -- this allows things like <T extends Something> which would be `TyRel Extends (TyRef (RefLocal "T")) (TyRef (RefLocal "Something"))`
    deriving (Show, Eq)

data RefIR
    = RefSpecialIR SpecialRefIR
    | RefLocalIR NameIR
    | RefQualIR QualNameIR
    deriving (Show, Eq)

data SpecialRefIR
    = SuperIR
    | ThisIR
    deriving (Show, Eq)

data TypeRelIR
    = InheritsIR
    | ExtendsIR
    | EqualsIR
    deriving (Show, Eq)

data NameSpaceIR = NameSpaceIR [String]
    deriving (Show, Eq)

data NameIR = NameIR String
    deriving (Show, Eq)

data ImportIR = ImportIR [String]
    deriving (Show, Eq)

data AnnotationIR = AnnotationIR NameIR
    deriving (Show, Eq)

data QualNameIR = QualNameIR NameSpaceIR NameIR
    deriving (Show, Eq)

data IntConstantIR = IntConstantIR Integer

data BlockIR
    = InlineIR ExprIR
    | DoBlockIR StmtIR
    deriving (Show, Eq)

data ExprIR
    = BlockExprIR [ExprIR]
    | IdentifierIR NameIR
    | MethodCallIR NameIR ExprIR
    | NewClassInstanceIR TypeIR ExprIR (Maybe StmtIR)
    | StringLiteralIR String
    | TernaryIR ExprIR ExprIR ExprIR
    | TupleIR ExprIR
    | BoolConstIR Bool
    | NotIR ExprIR
    | BBinaryIR BBinOpIR ExprIR ExprIR
    | RBinaryIR RBinOpIR ExprIR ExprIR
    | IntConstIR Integer
    | DoubleConstIR Scientific
    | FloatConstIR Double
    | LongConstIR Scientific
    | NegIR ExprIR
    | ABinaryIR ABinOpIR ExprIR ExprIR
    | ArrayIR ArrayOpIR ExprIR ExprIR
    | SpecialRefAsExprIR SpecialRefIR
    deriving (Show, Eq)

data StmtIR
    = ForIR ExprIR ExprIR ExprIR StmtIR
    | WhileIR ExprIR StmtIR
    | IfIR ExprIR StmtIR (Maybe StmtIR)
    | AssignIR NameIR (Maybe TypeIR) Bool BlockIR
    | AssignMultipleIR [NameIR] (Maybe TypeIR) Bool BlockIR
    | ReassignIR NameIR BlockIR
    | ReturnIR StmtIR
    | LambdaIR [FieldIR] BlockIR
    | ModelDefIR ModelIR
    | MethodDefIR MethodIR
    | ExprAsStmtIR ExprIR
    | BlockStmtIR [StmtIR]
    | MatchIR ExprIR [CaseIR]
    | PrintIR ExprIR JVM.ClassFile.FieldType
    | PrintlnIR ExprIR JVM.ClassFile.FieldType
    deriving (Show, Eq)

data CaseIR
    = CaseIR ExprIR BlockIR
    deriving (Show, Eq)

data BBinOpIR
    = AndIR
    | OrIR
    deriving (Show, Eq)


data ArrayOpIR
    = ArrayAppendIR
    deriving (Show, Eq)

data RBinOpIR
    = GreaterEqualIR
    | GreaterIR
    | LessEqualIR
    | LessIR
    deriving (Show, Eq)

data ABinOpIR
    = AddIR JVM.ClassFile.FieldType
    | SubtractIR JVM.ClassFile.FieldType
    | MultiplyIR JVM.ClassFile.FieldType
    | DivideIR JVM.ClassFile.FieldType
    deriving (Show, Eq)
