module AST.AST where

import Data.Scientific

import AST.IR

data Module = Module ModuleHeader [Model]
    deriving (Show)

moduleToModuleIR :: Module -> ModuleIR
moduleToModuleIR (Module header models) = ModuleIR (moduleHeaderToModuleHeaderIR header) $ map modelToModelIR models

data ModuleHeader = ModuleHeader
    { modNameSpace :: NameSpace
    , modImports :: [Import]
    }
    deriving (Show, Eq)

moduleHeaderToModuleHeaderIR :: ModuleHeader -> ModuleHeaderIR
moduleHeaderToModuleHeaderIR (ModuleHeader modNameSpace modImports) = ModuleHeaderIR (nameSpaceToNameSpaceIR modNameSpace) $ map importToImportIR modImports

data Method = Method
    { methodName :: Name
    , methodAnns :: [Annotation]
    , methodParams :: [Field]
    , methodModifiers :: [Modifier]
    , methodReturnType :: (Maybe Type)
    , methodBody :: Block
    }
    deriving (Show, Eq)

data Constant = Constant
    deriving (Show, Eq)

data ModelType
    = ClassModel
    | ObjectModel
    | TraitModel
    | UnknownModel
    deriving (Eq, Show)

modelTypeToModelTypeIR :: ModelType -> ModelTypeIR
modelTypeToModelTypeIR modelType = case modelType of
                                       ClassModel   -> ClassModelIR
                                       ObjectModel  -> ObjectModelIR
                                       TraitModel   -> TraitModelIR
                                       UnknownModel -> UnknownModelIR

data Modifier
    = Public
    | Protected
    | Private
    | PackageLocal
    | Abstract
    | Final
    deriving (Eq, Show)

modifierToModifierIR :: Modifier -> ModifierIR
modifierToModifierIR modifier = case modifier of
                                    Public       -> PublicIR
                                    Protected    -> ProtectedIR
                                    Private      -> PrivateIR
                                    PackageLocal -> PackageLocalIR
                                    Abstract     -> AbstractIR
                                    Final        -> FinalIR

data Model = Model
    { modelName :: Name
    , modelType :: ModelType
    , modelModifiers :: [Modifier]
    , modelFields :: [Field]
    , modelParent :: Maybe Type
    , modelParentArguments :: [Stmt]
    , modelInterfaces :: [Type]
    , modelBody :: Stmt
    }
    deriving (Show, Eq)

modelToModelIR :: Model -> ModelIR
modelToModelIR (Model modelName modelType modelModifiers modelFields modelParent modelParentArguments modelInterfaces modelBody) = do
    let parent = case modelParent of
                     Just a  -> Just $ typeToTypeIR a
                     Nothing -> Nothing
    ModelIR (nameToNameIR modelName) (modelTypeToModelTypeIR modelType) (map modifierToModifierIR modelModifiers) (map fieldToFieldIR modelFields) (parent) (map stmtToStmtIR modelParentArguments) (map typeToTypeIR modelInterfaces) (stmtToStmtIR modelBody)

data Field = Field
    { fieldName :: Name
    , fieldType :: Maybe Type
    , fieldInit :: Maybe Expr
    }
    deriving (Show, Eq)

fieldToFieldIR :: Field -> FieldIR
fieldToFieldIR (Field fieldName fieldType fieldInit) = do
    let fieldTypeIR = case fieldType of
                     Just a  -> Just $ typeToTypeIR a
                     Nothing -> Nothing
    let fieldInitIR = case fieldInit of
                     Just a  -> Just $ exprToExprIR a
                     Nothing -> Nothing
    FieldIR (nameToNameIR fieldName) (fieldTypeIR) (fieldInitIR)

data Type
    = Init
    | TypeRef Ref
    | TypeApp Ref [Type] -- type application, aka Map<A,B> -> `TyApp (RefLocal "Map") [TyRef (RefLocal "A"), TyRef (RefLocal "B")]`
    | TypeRel TypeRel Type Type -- this allows things like <T extends Something> which would be `TyRel Extends (TyRef (RefLocal "T")) (TyRef (RefLocal "Something"))`
    deriving (Show, Eq)

typeToTypeIR :: Type -> TypeIR
typeToTypeIR Init =  InitIR
typeToTypeIR (TypeRef ref) = TypeRefIR (refToRefIR ref)
typeToTypeIR (TypeApp ref types) = TypeAppIR (refToRefIR ref) (map typeToTypeIR types)
typeToTypeIR (TypeRel typeRel t1 t2) = TypeRelIR (typeRelToTypeRelIR typeRel) (typeToTypeIR t1) (typeToTypeIR t2)

data Ref
    = RefSpecial SpecialRef
    | RefLocal Name
    | RefQual QualName
    -- | RefOp AOperator
    deriving (Show, Eq)

refToRefIR :: Ref -> RefIR
refToRefIR (RefSpecial specialRef) = RefSpecialIR (specialRefToSpecialRefIR specialRef)
refToRefIR (RefLocal name) = RefLocalIR (nameToNameIR name)
refToRefIR (RefQual qualName) = RefQualIR (qualNameToQualNameIR qualName)

data SpecialRef
    = Super
    | This
    deriving (Show, Eq)

specialRefToSpecialRefIR :: SpecialRef -> SpecialRefIR
specialRefToSpecialRefIR Super = SuperIR
specialRefToSpecialRefIR This = ThisIR

data TypeRel
    = Inherits
    | Extends
    | Equals
    deriving (Show, Eq)

typeRelToTypeRelIR :: TypeRel -> TypeRelIR
typeRelToTypeRelIR Inherits = InheritsIR
typeRelToTypeRelIR Extends = ExtendsIR
typeRelToTypeRelIR Equals = EqualsIR

data NameSpace = NameSpace [String]
    deriving (Show, Eq)

nameSpaceToNameSpaceIR :: NameSpace -> NameSpaceIR
nameSpaceToNameSpaceIR (NameSpace nameSpace) = NameSpaceIR nameSpace

data Name = Name String
    deriving (Show, Eq)

nameToNameIR :: Name -> NameIR
nameToNameIR (Name value) = NameIR value

data Import = Import [String]
    deriving (Show, Eq)

importToImportIR :: Import -> ImportIR
importToImportIR (Import location) = ImportIR location

data Annotation = Annotation Name
    deriving (Show, Eq)

annotationToAnnotationIR :: Annotation -> AnnotationIR
annotationToAnnotationIR (Annotation name) = AnnotationIR (nameToNameIR name)

data QualName = QualName NameSpace Name
    deriving (Show, Eq)

qualNameToQualNameIR :: QualName -> QualNameIR
qualNameToQualNameIR (QualName nameSpace name) = QualNameIR (nameSpaceToNameSpaceIR nameSpace) (nameToNameIR name)

data IntConstant = IntConstant Integer

data Block
    = Inline Expr
    | DoBlock Stmt
    deriving (Show, Eq)

data Expr
    = BlockExpr [Expr]
    | Identifier Name
    | MethodCall Name Expr
    | NewClassInstance Type Expr (Maybe Stmt)
    | StringLiteral String
    | Ternary Expr Expr Expr
    | Tuple Expr
    | BoolConst Bool
    | Not Expr
    | BBinary BBinOp Expr Expr
    | RBinary RBinOp Expr Expr
    | IntConst Integer
    | DoubleConst Scientific
    | FloatConst Float
    | LongConst Integer
    | Neg Expr
    | ABinary ABinOp Expr Expr
    | Array ArrayOp Expr Expr
    | SpecialRefAsExpr SpecialRef
    deriving (Show, Eq)

exprToExprIR :: Expr -> ExprIR
exprToExprIR (BlockExpr expressions) = BlockExprIR (map exprToExprIR expressions)

data Stmt
    = For Expr Expr Expr Stmt
    | While Expr Stmt
    | If Expr Stmt (Maybe Stmt)
    | TryBlock ExceptionHandler (Maybe ExceptionHandler) (Maybe ExceptionHandler)
    | Assign Name (Maybe Type) Bool Block
    | AssignMultiple [Name] (Maybe Type) Bool Block
    | Reassign Name Block
    | Return Stmt
    | Lambda [Field] Block
    | ModelDef Model
    | MethodDef Method
    | ExprAsStmt Expr
    | BlockStmt [Stmt]
    | Match Expr [Case]
    deriving (Show, Eq)

stmtToStmtIR :: Stmt -> StmtIR
stmtToStmtIR (For e1 e2 e3 stmt) = ForIR (exprToExprIR e1) (exprToExprIR e2) (exprToExprIR e3) (stmtToStmtIR stmt)

data Case
    = Case Expr Block
    deriving (Show, Eq)

data ExceptionHandler
    = TryStatement Stmt
    | CatchStatement [Field] Stmt
    | FinallyStatement Stmt
    deriving (Show, Eq)

data BBinOp
    = And
    | Or
    deriving (Show, Eq)


data ArrayOp
    = ArrayAppend
    deriving (Show, Eq)

data RBinOp
    = GreaterEqual
    | Greater
    | LessEqual
    | Less
    deriving (Show, Eq)

data ABinOp
    = Add
    | Subtract
    | Multiply
    | Divide
    deriving (Show, Eq)
