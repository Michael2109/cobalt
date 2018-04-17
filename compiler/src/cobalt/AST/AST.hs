module AST.AST where

-- Yes, definitely split into related groups. This is a rough cut of
-- what it *could* look like
data Module = Module ModHeader [Def]


-- data Module e = Module ModHeader [Def e]

data ModHeader = ModHeader
    { modName :: NameSpace
    , modImports :: [Import]
    }
    deriving (Show, Eq)
{--
data Def = Def Name (DefExpr e)

data DefExpr e
    = ConstDef e
    | ClassDef (Class e)
    | FunDef (Function e)

data Function = Function
    { funName :: Name
    , funAnns :: [Annotation]
    , funParams :: [Param]
    , funBody :: e
    }
--}
data Def = Def Name DefExpr
    deriving (Show, Eq)

data DefExpr
    = ConstDef Expr
    | ClassDef Class
    | MethodDef Method
    deriving (Show, Eq)

data Method = Method
    { methodName :: Name
    , methodAnns :: [Annotation]
    , methodParams :: [Field]
    , methodReturnType :: Type
    , methodBody :: Expr
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
    | Abstract
    | Final
    deriving (Eq, Show)

data Class = Class
    { className :: Name
    , classFields :: [Field]
    , classMethods :: [Method]
    }
    deriving (Show, Eq)

data Field = Field
    { fieldName :: Name
    , fieldType :: Type
    , fieldInit :: Maybe Expr
    }
    deriving (Show, Eq)

data Type
    = TypeRef Ref
    | TypeApp Ref [Type] -- type application, aka Map<A,B> -> `TyApp (RefLocal "Map") [TyRef (RefLocal "A"), TyRef (RefLocal "B")]`
    | TypeRel TypeRel Type Type -- this allows things like <T extends Something> which would be `TyRel Extends (TyRef (RefLocal "T")) (TyRef (RefLocal "Something"))`
    deriving (Show, Eq)

data Ref
    = RefSpecial SpecialRef
    | RefLocal Name
    | RefQual QualName
    | RefOp Operator
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

data NameSpace = NameSpace
    deriving (Show, Eq)

data Name = Name String
    deriving (Show, Eq)

data Import = Import [String]
    deriving (Show, Eq)

data Annotation = Annotation Name
    deriving (Show, Eq)


{--
data Expr
    = Call Expr [Expr]
    | Assign Name Expr Expr
    | While Expr Expr
    | For ...
    | DefE Name DefExpr
    | ... --}

data QualName = QualName NameSpace Name
    deriving (Show, Eq)

data Operator
    -- either make them fixed:
    = Plus
    | Minus
    | Shift
    deriving (Show, Eq)
    -- | ...
    -- or allow user built operators
    -- = Operator QualName -- (for instance)

{--builtinNS :: NameSpace
builtinNS = "<BUILTIN>"
plus, minus, shift :: Operator
plus = QualName builtinNS "+"
minus = QualName builtinNS "-"
shift = QualName builtinNS ">>" --}

-- or if you prefer statement based programming

data Expr
    = Call Expr [Expr]
    | Ternary Expr Expr Expr
    -- | ...
    | Block [Stmt]
    deriving (Show, Eq)

data Stmt
    = For Expr
    | While Expr
    | Assign Name Expr
    | BareExpr Expr
    | Return Expr
    | DefStmt Name DefExpr
    | Identifier Name
    deriving (Show, Eq)
    -- | ...

-- and funBody would then be of type [Stmt]


-- important to note is the `Call` stuff in the `Expr` types. It can
-- be used to implement *both* regular function calls as well as
-- operator uses. For instance `a + b` is `Call (RefOp Plus) [RefLocal
-- "a", RefLocal "b"]`. You still have to perhaps handle some ops and
-- functions specially (builtins) but you can reuse a lot of code gen
-- machinery.

-- You can even make `Call` do `new` by having another kind of
-- `SpecialRef` and then `new ClsExpr(params)` becomes `Call
-- (RefSpecial New) [params]`


-- On stuff like class declarations, imports etc: What I did here was
-- separate them. You *can* make them `Expr`, that allows users to
-- define them inline, in a function for instance, which is cool as it
-- unifies structural elements, like classes and "regular" code, and
-- it enables a lot of cool stuff but is hard to implement (right). I
-- included those here with `DefStmt` and `DefExpr`.
-- You could then write something like
--

-- someVal = "dummy"
-- anon = class {
--            private a String;
--            constructor (String a) { this.a = a; }
--            public String getStr() { return someVal + a; }
--        };
-- and then you can use that as a regular class
--
-- anonInstance = new anon("test");
-- anoninstance.getStr(); => "dummytest"

-- stuff like `Parentheses` don't need to be in the ast. They are only
-- important during parsing.

-- If you cannot immediately parse into this simplified IR then try
-- using multiple passes, gradually simplifying different types of
-- `Expr`s etc.  This is relatively easy when you make `Function`
-- `Module` etc polymorphic over the type of expression, like so

