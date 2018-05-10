module AST.AST where

import Data.Scientific

data Module = Module ModuleHeader [Model]
    deriving (Show)

data ModuleHeader = ModuleHeader
    { modName :: NameSpace
    , modImports :: [Import]
    }
    deriving (Show, Eq)

data Method = Method
    { methodName :: Name
    , methodAnns :: [Annotation]
    , methodParams :: [Field]
    , methodModifiers :: [Modifier]
    , methodReturnType :: (Maybe Type)
    , methodBody :: Assignment
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

data Name = Name String
    deriving (Show, Eq)

data Import = Import [String]
    deriving (Show, Eq)

data Annotation = Annotation Name
    deriving (Show, Eq)

data QualName = QualName NameSpace Name
    deriving (Show, Eq)

data IntConstant = IntConstant Integer

data Assignment
    = ExprAssignment Expr
    | StmtAssignment Stmt
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
    deriving (Show, Eq)

data Stmt
    = For Expr Expr Expr Stmt
    | While Expr Stmt
    | If Expr Stmt (Maybe Stmt)
    | TryBlock ExceptionHandler (Maybe ExceptionHandler) (Maybe ExceptionHandler)
    | Assign Name (Maybe Type) Bool Assignment
    | AssignMultiple [Name] (Maybe Type) Bool Assignment
    | Reassign Name Assignment
    | Return Stmt
    | Lambda [Field] Assignment
    | ModelDef Model
    | MethodDef Method
    | ExprAsStmt Expr
    | BlockStmt [Stmt]
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
