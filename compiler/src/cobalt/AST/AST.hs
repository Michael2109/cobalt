module AST.AST where

import Data.Scientific

import AST.IR

class AST2IR a where
    astToIR :: a -> b

data Module = Module ModuleHeader [Model]
    deriving (Show)

instance AST2IR Module where
    astToIR (Module header modules) = ModuleIR (astToIR header) $ map astToIR modules

data ModuleHeader = ModuleHeader
    { modNameSpace :: NameSpace
    , modImports :: [Import]
    }
    deriving (Show, Eq)

instance AST2IR ModuleHeader where
    astToIR (ModuleHeader modNameSpace modImports) = ModuleHeaderIR (astToIR modNameSpace) $ map astToIR modImports

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

data Modifier
    = Public
    | Protected
    | Private
    | PackageLocal
    | Abstract
    | Final
    deriving (Eq, Show)

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

data Field = Field
    { fieldName :: Name
    , fieldType :: Maybe Type
    , fieldInit :: Maybe Expr
    }
    deriving (Show, Eq)

data Type
    = Init
    | TypeRef Ref
    | TypeApp Ref [Type] -- type application, aka Map<A,B> -> `TyApp (RefLocal "Map") [TyRef (RefLocal "A"), TyRef (RefLocal "B")]`
    | TypeRel TypeRel Type Type -- this allows things like <T extends Something> which would be `TyRel Extends (TyRef (RefLocal "T")) (TyRef (RefLocal "Something"))`
    deriving (Show, Eq)

data Ref
    = RefSpecial SpecialRef
    | RefLocal Name
    | RefQual QualName
    -- | RefOp AOperator
    deriving (Show, Eq)

data SpecialRef
    = Super
    | This
    deriving (Show, Eq)

data TypeRel
    = Inherits
    | Extends
    | Equals
    deriving (Show, Eq)

data NameSpace = NameSpace [String]
    deriving (Show, Eq)

instance AST2IR NameSpace where
    astToIR (NameSpace nameSpace) = NameSpaceIR nameSpace

data Name = Name String
    deriving (Show, Eq)

data Import = Import [String]
    deriving (Show, Eq)

instance AST2IR Import where
    astToIR (Import location) = ImportIR location

data Annotation = Annotation Name
    deriving (Show, Eq)

data QualName = QualName NameSpace Name
    deriving (Show, Eq)

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
