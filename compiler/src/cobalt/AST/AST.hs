module AST.AST where

import Data.Scientific

import AST.IR
import qualified  Java.Lang
import qualified JVM.ClassFile

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
    , methodFields :: [Field]
    , methodModifiers :: [Modifier]
    , methodReturnType :: (Maybe Type)
    , methodBody :: Block
    }
    deriving (Show, Eq)

methodToMethodIR :: Method -> MethodIR
methodToMethodIR (Method methodName methodAnns methodFields methodModifiers methodReturnType methodBody) = do
    let methodReturnTypeIR = case methodReturnType of
                                 Just a  -> Just $ typeToTypeIR a
                                 Nothing -> Nothing
    MethodIR (nameToNameIR methodName) (map annotationToAnnotationIR methodAnns) (map fieldToFieldIR methodFields) (map modifierToModifierIR methodModifiers) methodReturnTypeIR (blockToBlockIR methodBody)

data Constant = Constant
    deriving (Show, Eq)

constantToConstantIR :: Constant -> ConstantIR
constantToConstantIR Constant = ConstantIR

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
    | Pure
    deriving (Eq, Show)

modifierToModifierIR :: Modifier -> ModifierIR
modifierToModifierIR modifier = case modifier of
                                    Public       -> PublicIR
                                    Protected    -> ProtectedIR
                                    Private      -> PrivateIR
                                    PackageLocal -> PackageLocalIR
                                    Abstract     -> AbstractIR
                                    Final        -> FinalIR
                                    Pure         -> PureIR

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

blockToBlockIR :: Block -> BlockIR
blockToBlockIR (Inline expression) = InlineIR (exprToExprIR expression)
blockToBlockIR (DoBlock statement) = DoBlockIR (stmtToStmtIR statement)

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
    | FloatConst Double
    | LongConst Scientific
    | Neg Expr
    | ABinary ABinOp Expr Expr
    | Array ArrayOp Expr Expr
    | SpecialRefAsExpr SpecialRef
    deriving (Show, Eq)

exprToExprIR :: Expr -> ExprIR
exprToExprIR (BlockExpr expressions) = BlockExprIR (map exprToExprIR expressions)
exprToExprIR (Identifier name) = IdentifierIR (nameToNameIR name)
exprToExprIR (MethodCall name expression) = MethodCallIR (nameToNameIR name) (exprToExprIR expression)
exprToExprIR (NewClassInstance classType expression anonymousClassStmt) = do
    let anonymousClassStmtIR = case anonymousClassStmt of
                                   Just stmt -> Just $ stmtToStmtIR stmt
                                   Nothing   -> Nothing
    NewClassInstanceIR (typeToTypeIR classType) (exprToExprIR expression) anonymousClassStmtIR
exprToExprIR (StringLiteral value) = StringLiteralIR value
exprToExprIR (Ternary condition ifExpr elseExpr) = TernaryIR (exprToExprIR condition) (exprToExprIR ifExpr) (exprToExprIR elseExpr)
exprToExprIR (Tuple expression) = TupleIR (exprToExprIR expression)
exprToExprIR (BoolConst value) = BoolConstIR value
exprToExprIR (Not expression) = NotIR (exprToExprIR expression)
exprToExprIR (BBinary op expr1 expr2) = BBinaryIR (bBinOpToBBinOpIR op) (exprToExprIR expr1) (exprToExprIR expr2)
exprToExprIR (RBinary op expr1 expr2) = RBinaryIR (rBinOpToRBinOpIR op) (exprToExprIR expr1) (exprToExprIR expr2)
exprToExprIR (IntConst value) = IntConstIR value
exprToExprIR (DoubleConst value) = DoubleConstIR value
exprToExprIR (FloatConst value) = FloatConstIR value
exprToExprIR (LongConst value) = LongConstIR value
exprToExprIR (Neg expression) = NegIR (exprToExprIR expression)
exprToExprIR (ABinary op expr1 expr2) = ABinaryIR (aBinOpToABinOpIR op  (getExpressionType expr1)) (exprToExprIR expr1) (exprToExprIR expr2)
exprToExprIR (Array op expr1 expr2) = ArrayIR (arrayOpToArrayOpIR op) (exprToExprIR expr1) (exprToExprIR expr2)
exprToExprIR (SpecialRefAsExpr specialRef) = SpecialRefAsExprIR (specialRefToSpecialRefIR specialRef)

data Stmt
    = For Expr Expr Expr Stmt
    | While Expr Stmt
    | If Expr Stmt (Maybe Stmt)
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
    | Print Expr
    | Println Expr
    deriving (Show, Eq)

stmtToStmtIR :: Stmt -> StmtIR
stmtToStmtIR (For e1 e2 e3 stmt) = ForIR (exprToExprIR e1) (exprToExprIR e2) (exprToExprIR e3) (stmtToStmtIR stmt)
stmtToStmtIR (While expression statement) = WhileIR (exprToExprIR expression) (stmtToStmtIR statement)
stmtToStmtIR (If expression statement elseStatement) = do
    let elseStatementIR = case elseStatement of
                              Just stmt -> Just $ stmtToStmtIR stmt
                              Nothing   -> Nothing
    IfIR (exprToExprIR expression) (stmtToStmtIR statement) elseStatementIR
stmtToStmtIR (Assign name valType immutable block) = do
    let valTypeIR = case valType of
                              Just t  -> Just $ typeToTypeIR t
                              Nothing -> Nothing
    AssignIR (nameToNameIR name) valTypeIR immutable (blockToBlockIR block)
stmtToStmtIR (AssignMultiple names valType immutable block) = do
    let valTypeIR = case valType of
                              Just t  -> Just $ typeToTypeIR t
                              Nothing -> Nothing
    AssignMultipleIR (map nameToNameIR names) valTypeIR immutable (blockToBlockIR block)
stmtToStmtIR (Reassign name block) = ReassignIR (nameToNameIR name) (blockToBlockIR block)
stmtToStmtIR (Return statement) = ReturnIR (stmtToStmtIR statement)
stmtToStmtIR (ModelDef model) = ModelDefIR (modelToModelIR model)
stmtToStmtIR (MethodDef method) = MethodDefIR (methodToMethodIR method)
stmtToStmtIR (ExprAsStmt expression) = ExprAsStmtIR (exprToExprIR expression)
stmtToStmtIR (BlockStmt statements) = BlockStmtIR (map stmtToStmtIR statements)
stmtToStmtIR (Match expression cases) = MatchIR (exprToExprIR expression) (map caseToCaseIR cases)
stmtToStmtIR (Print expression) = PrintIR (exprToExprIR expression) (getExpressionType expression)
stmtToStmtIR (Println expression) = PrintlnIR (exprToExprIR expression) (getExpressionType expression)

data Case
    = Case Expr Block
    deriving (Show, Eq)

caseToCaseIR :: Case -> CaseIR
caseToCaseIR (Case expression block) = CaseIR (exprToExprIR expression) (blockToBlockIR block)

data BBinOp
    = And
    | Or
    deriving (Show, Eq)

bBinOpToBBinOpIR :: BBinOp -> BBinOpIR
bBinOpToBBinOpIR And = AndIR
bBinOpToBBinOpIR Or = OrIR

data ArrayOp
    = ArrayAppend
    deriving (Show, Eq)

arrayOpToArrayOpIR :: ArrayOp -> ArrayOpIR
arrayOpToArrayOpIR ArrayAppend = ArrayAppendIR

data RBinOp
    = GreaterEqual
    | Greater
    | LessEqual
    | Less
    deriving (Show, Eq)

rBinOpToRBinOpIR :: RBinOp -> RBinOpIR
rBinOpToRBinOpIR GreaterEqual = GreaterEqualIR
rBinOpToRBinOpIR Greater = GreaterIR
rBinOpToRBinOpIR LessEqual = LessEqualIR
rBinOpToRBinOpIR Less = LessIR

data ABinOp
    = Add
    | Subtract
    | Multiply
    | Divide
    deriving (Show, Eq)

aBinOpToABinOpIR :: ABinOp -> JVM.ClassFile.FieldType -> ABinOpIR
aBinOpToABinOpIR Add expressionType = AddIR expressionType
aBinOpToABinOpIR Subtract expressionType = SubtractIR expressionType
aBinOpToABinOpIR Multiply expressionType = MultiplyIR expressionType
aBinOpToABinOpIR Divide expressionType = DivideIR expressionType

-- Restructures the AST
restructureModule (Module header models) = Module header (map restructureModel models)
restructureModel (Model modelName modelType modelModifiers modelFields modelParent modelParentArguments modelInterfaces modelBody) = Model modelName modelType modelModifiers modelFields modelParent modelParentArguments modelInterfaces modelBody

restructureStmt (MethodDef method) = MethodDef method

-- Utils
extractName :: Name -> String
extractName (Name name) = name

extractModuleHeader (Module header _) = header

extractModuleHeaderNameSpace (ModuleHeader nameSpace _) = nameSpace

extractNameSpaceValue (NameSpace nameSpace) = nameSpace

getExpressionType (StringLiteral _)         = Java.Lang.stringClass
getExpressionType (IntConst _)              = JVM.ClassFile.IntType
getExpressionType (LongConst _)             = JVM.ClassFile.LongInt
getExpressionType (FloatConst _)            = JVM.ClassFile.FloatType
getExpressionType (DoubleConst _)           = JVM.ClassFile.DoubleType
getExpressionType (ABinary op expr1 expr2)  = getExpressionType expr1
getExpressionType (e)                       = error $ show e
