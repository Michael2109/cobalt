{-|
Module      : ExprParser
Description : Parses all expressions.
The highest level parser that uses functions in the BaseParser and ABExprParser to generate the AST.
-}
module Parser.ExprParser where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)

import AST.AST
--import AST.Block
--import AST.Data.ModelType
--import AST.Data.Modifier
import Parser.BaseParser
import Parser.ParserType
import SymbolTable.SymbolTable

{--
aTerm :: Parser Expr
aTerm = parens aExpr
    <|> classVariableParser
    <|> newClassInstanceParser
    <|> objectMethodCallParser
    <|> methodCallParser
    <|> identifierParser
    <|> IntConst <$> integerParser
    <|> DoubleConstant <$> doubleParser

aOperators :: [[Operator Parser Expr]]
aOperators =
    [ [Prefix (Neg <$ symbol "-") ]
    , [ InfixL (ABinary Multiply <$ symbol "*")
      , InfixL (ABinary Divide   <$ symbol "/") ]
    , [ InfixL (ABinary Add      <$ symbol "+")
      , InfixL (ABinary Subtract <$ symbol "-") ]
    ]

aExpr :: Parser Expr
aExpr = makeExprParser aTerm aOperators

bTerm :: Parser Expr
bTerm = BoolConst True  <$ rword "True"
    <|> BoolConst False <$ rword "False"
    <|> parens bExpr
    <|> try rExpr
    <|> classVariableParser
    <|> newClassInstanceParser
    <|> objectMethodCallParser
    <|> methodCallParser
    <|> identifierParser

bOperators :: [[Operator Parser Expr]]
bOperators =
    [ [ Prefix (Not <$ rword "not") ]
    , [ InfixL (BBinary And <$ rword "and")
      , InfixL (BBinary Or <$ rword "or") ]
    ]

bExpr :: Parser Expr
bExpr = makeExprParser bTerm bOperators

rExpr :: Parser Expr
rExpr = do
    try $ do
        a1 <- aExpr
        op <- relation
        a2 <- aExpr
        return (RBinary op a1 a2)

relation :: Parser Expr
relation = (symbol ">=" *> pure GreaterEqual)
    <|> (symbol "<=" *> pure LessEqual)
    <|> (symbol ">" *> pure Greater)
    <|> (symbol "<" *> pure Less)

modelParser :: Parser Expr
modelParser = try $ L.nonIndented scn p
  where
    p = do
        package <- optional packageParser
        imports <- many importParser
        optional $ L.lineFold scn $ \sp' -> return ()
        modifiers <- many $ choice [accessModifierParser, abstractModifierParser, finalModifierParser]
        modelType <- modelTypeParser
        name <- identifier
        typeParam <- optional typeParameterParser
        paramsOpt <- optional $ parens $ sepBy parameterParser (symbol ",")
        let params = case paramsOpt of
                             Just ps -> ps
                             Nothing -> []
        extendsKeyword <- optional $ rword "extends"
        parent <- optional $ identifier
        parentArgsOpt <- optional $ parens $ sepBy argumentParser (symbol ",")
        let parentArgs = case parentArgsOpt of
                             Just ps -> ps
                             Nothing -> []
        implementsKeyword <- optional $ rword "implements"
        interfaces <- sepBy identifier (symbol ",")
        modifierBlocks <- many $ try $ modifierBlockParser False
        constructorExprs <- many $ expr'
        exprs <- many (methodParser name False <|> expr')

        return $
            case modelType of
                ClassModel  -> (Class package name typeParam modifiers params parent parentArgs interfaces imports modifierBlocks constructorExprs exprs)
                ObjectModel -> (Object package name typeParam modifiers params parent parentArgs interfaces imports modifierBlocks constructorExprs exprs)
                TraitModel  -> (Trait package name typeParam modifiers params parent parentArgs interfaces imports modifierBlocks constructorExprs exprs)

packageParser :: Parser Expr
packageParser = try $ L.nonIndented scn p
  where
    p = do
        try (rword "package")
        locations <- sepBy1 identifier (symbol ".")
        return $ (Package locations)

importParser :: Parser Expr
importParser = try $ L.nonIndented scn p
  where
    p = do
        try (rword "import")
        locations <- sepBy1 identifier (symbol ".")
        return $ (Import locations)

typeParameterParser :: Parser Expr
typeParameterParser = do
    try $ symbol "["
    typeName <- identifierParser
    symbol "]"
    return $ TypeParameter typeName

parameterParser :: Parser Expr
parameterParser = do
    varName  <- identifierParser
    symbol ":"
    varType <- identifierParser
    return $ Parameter varType varName

methodParser :: String -> Bool -> Parser Expr
methodParser moduleName static = try $ L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
        annotations <- try (optional annotationParser)
        modifiers <- many $ choice [accessModifierParser, abstractModifierParser, finalModifierParser]

        name <- identifierParser
        fullParams <- optional $ parens $ sepBy parameterParser (symbol ",")
        let params = case fullParams of
                         Just ps -> ps
                         Nothing -> []
        symbol ":"
        rType <- identifierParser
        return (L.IndentMany Nothing (return . (Method name annotations modifiers params rType static)) (expr'))

identifierParser :: Parser Expr
identifierParser = do
    name <- identifier
    return $ Identifier name

thisVarParser :: Parser Expr
thisVarParser = do
    try $ do
        rword "this"
        symbol "."
    name <- identifierParser
    return $ ThisVar name

arithmeticParser :: Parser Expr
arithmeticParser = do
    aE <- aExpr
    return aE

booleanParser :: Parser Expr
booleanParser = do
    bE <- try bExpr
    return bE

stringLiteralParser :: Parser Expr
stringLiteralParser = do
    value <- char '"' >> manyTill L.charLiteral (char '"')
    return $ StringLiteral value

stringLiteralMultilineParser :: Parser Expr
stringLiteralMultilineParser = do
    symbol "```"
    contents <- many $ L.lineFold scn $ \sp' -> some L.charLiteral
    symbol "```"
    return $ StringLiteral $ intercalate "\n" contents

assignParser :: Parser Expr
assignParser = do
    try (rword "let")
    mutableOpt <- optional $ rword "mutable"
    let immutable = case mutableOpt of
                        Nothing -> True
                        Just _  -> False
    varName <- identifierParser
    varType <- optional $ do
        symbol ":"
        vType <- valueTypeParser
        return vType
    symbol "="
    e <- argumentParser
    return $ Assign immutable varType varName e

reassignParser :: Parser Expr
reassignParser = do
    name  <- try $ do
        id <- identifierParser <|> thisVarParser
        symbol "<-"
        return id
    value <- argumentParser
    return (Reassign name value)

arrayElementSelect :: Parser Expr
arrayElementSelect = do
    symbol "!!"
    elementNum <- word
    return $ ArrayElementSelect elementNum

arrayAppend :: Parser Expr
arrayAppend = do
    arrays <- sepBy1 (expr') (symbol "++")
    return $ ArrayAppend arrays

thisMethodCallParser :: Parser Expr
thisMethodCallParser =
    try $ do
        rword "this"
        symbol "."
        methodName <- identifier
        args <- parens $ sepBy (argumentParser) (symbol ",")
        return $ ThisMethodCall methodName args

superMethodCallParser :: Parser Expr
superMethodCallParser =
    try $ do
        rword "super"
        symbol "."
        methodName <- identifier
        args <- parens $ sepBy (argumentParser) (symbol ",")
        return $ SuperMethodCall methodName args

methodCallParser :: Parser Expr
methodCallParser =
    try $ do
        methodName <- identifier
        args <- parens $ sepBy (argumentParser) (symbol ",")
        return $ MethodCall methodName args

objectMethodCallParser :: Parser Expr
objectMethodCallParser =
    try $ do
        objectName <- identifier
        symbol "."
        methodName <- identifier
        args <- parens $ sepBy (argumentParser) (symbol ",")
        return $ ObjectMethodCall objectName methodName args

newClassInstanceParser :: Parser Expr
newClassInstanceParser  = do
    try (rword "new")
    className <- identifierParser
    arguments <- parens $ sepBy (argumentParser) (symbol ",")
    return $ (NewClassInstance className arguments)

classVariableParser ::Parser Expr
classVariableParser  =
    try $ do
        className <- identifier
        symbol "."
        varName <- identifier
        return $ ClassVariable className varName

modifierBlockParser :: Bool -> Parser Expr
modifierBlockParser static = try $ L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
        modifier <- try (rword "public") <|> try (rword "protected") <|> try (rword "private")
        return (L.IndentMany Nothing (return . (ModifierBlock)) (assignParser))

annotationParser :: Parser Expr
annotationParser  = do
    try (symbol "@")
    name <- identifier
    return $ Annotation name

valueTypeParser :: Parser Expr
valueTypeParser = do
    value <- parameterizedTypeParser <|> identifierParser
    return $ Type value

parameterizedTypeParser :: Parser Expr
parameterizedTypeParser = do
    try $ do
        className <- identifierParser
        typeParameter <- typeParameterParser
        return $ ParameterizedType className typeParameter

argumentTypeParser :: Parser Expr
argumentTypeParser = do
    value <- identifier
    return $ ArgumentType value

returnType :: Parser Expr
returnType = do
    value <- identifier
    return $ ReturnType value

argumentParser :: Parser Expr
argumentParser = do
    value <- thisParser <|> classVariableParser <|> arithmeticParser <|> booleanParser <|> newClassInstanceParser <|> stringLiteralParser <|> stringLiteralMultilineParser <|> identifierParser
    return value

printParser :: Parser Expr
printParser  = do
    try $ rword "println"
    e <- parens argumentParser
    return $ Print e

ifStatementBlockParser :: Parser Expr
ifStatementBlockParser = do
    ifStmt <- ifStmtParser
    elifStmt <- optional elifStmtParser
    elseStmt <- optional elseStmtParser
    return $ IfStatement ifStmt elifStmt elseStmt

ifStmtParser :: Parser Expr
ifStmtParser  = try $ L.indentBlock scn p
  where
    p = do
        rword "if"
        cond  <- parens argumentParser
        return (L.IndentMany Nothing (return . (If cond)) (expr' <|> argumentParser))

elifStmtParser :: Parser Expr
elifStmtParser  = try $ L.indentBlock scn p
  where
    p = do
        rword "elif"
        cond  <- parens argumentParser
        return (L.IndentMany Nothing (return . (ElseIf cond)) (expr' <|> argumentParser))

elseStmtParser :: Parser Expr
elseStmtParser  = try $ L.indentBlock scn p
  where
    p = do
        rword "else"
        return (L.IndentMany Nothing (return . (Else)) (expr' <|> argumentParser))

whileParser :: Parser Expr
whileParser  = try $ L.indentBlock scn p
  where
    p = do
        rword "while"
        e <- parens (booleanParser )
        return (L.IndentMany Nothing (return . (While e)) (expr' ))

forLoopParser :: Parser Expr
forLoopParser  = try $ L.indentBlock scn p
  where
    p = do
        rword "for"
        symbol "("
        varName <- identifier
        symbol "<-"
        start <- arithmeticParser
        rword "to"
        end <- arithmeticParser
        symbol ")"
        return (L.IndentMany Nothing (return . (For varName start end)) (expr' ))

tryParser :: Parser Expr
tryParser  = try $ L.indentBlock scn p
  where
    p = do
        rword "try"
        return (L.IndentMany Nothing (return . (Try)) (expr' ))

catchParser :: Parser Expr
catchParser  = try $ L.indentBlock scn p
  where
    p = do
        rword "catch"
        fullParams <- optional $ parens $ sepBy parameterParser (symbol ",")
        let params = case fullParams of
                         Just ps -> ps
                         Nothing -> []
        return (L.IndentMany Nothing (return . (Catch params)) (expr' ))

whereStmt :: Parser Expr
whereStmt = do
    rword "where"
    symbol "{"
    exprs <- many expr
    symbol "}"
    return $ (Where exprs)

thisParser :: Parser Expr
thisParser = do
    try (rword "this")
    return This

superParser :: Parser Expr
superParser = do
    rword "super"
    return Super

expr :: Parser Expr
expr = f <$> sepBy1 (expr') (symbol ";")
  where
    -- if there's only one expr return it without using ‘Seq’
    f l = if length l == 1 then head l else Seq l

expr' :: Parser Expr
expr' =
    modelParser
    <|> forLoopParser
    <|> ifStatementBlockParser

    <|> whileParser

    <|> tryParser
    <|> catchParser
    <|> printParser
    <|> thisMethodCallParser
    <|> superMethodCallParser
    <|> objectMethodCallParser
    <|> methodCallParser
    <|> newClassInstanceParser
    <|> classVariableParser
    <|> try arrayElementSelect

    <|> assignParser
    <|> reassignParser

    <|> thisVarParser
    <|> thisParser

   <|> try whereStmt
--}



abstractModifierParser :: Parser Modifier
abstractModifierParser = Abstract <$ rword "abstract"

accessModifierParser :: Parser Modifier
accessModifierParser
    =   Public    <$ rword "public"
    <|> Protected <$ rword "protected"
    <|> Private   <$ rword "private"

annotationParser :: Parser Annotation
annotationParser = do
    symbol "@"
    name <- identifier
    return $ Annotation $ Name name

expressionParser :: Parser Expr
expressionParser
    = Block <$> many statementParser

fieldParser :: Parser Field
fieldParser = do
    name <- identifier
    symbol ":"
    varType <- identifier
    return $ Field (Name name) (TypeRef $ RefLocal $ Name varType) Nothing

finalModifierParser :: Parser Modifier
finalModifierParser = Final <$ rword "final"

identifierParser :: Parser Stmt
identifierParser = do
    name <- identifier
    return $ Identifier $ Name name

methodParser :: Parser Method
methodParser = try $ L.indentBlock scn p
  where
    p = do
      annotations <- many annotationParser
      modifiers <- many $ choice [accessModifierParser, abstractModifierParser, finalModifierParser]
      name <- identifier
      fields <- parens $ sepBy fieldParser $ symbol ","
      symbol ":"
      returnType <- typeRefParser
      return (L.IndentMany Nothing (return . (Method (Name name) annotations fields returnType) . Block) statementParser)

modelParser :: Parser Class
modelParser = try $ L.nonIndented scn p
  where
    p = do
        rword "class"
        name <- identifier
        return $ Class (Name name) [] []

modelTypeParser :: Parser ModelType
modelTypeParser
    =   ClassModel    <$ rword "class"
    <|> ObjectModel   <$ rword "object"
    <|> TraitModel    <$ rword "trait"

returnStatementParser :: Parser Stmt
returnStatementParser = do
    rword "return"
    name <- identifier
    expression <- expressionParser
    return $ Return expression

statementParser :: Parser Stmt
statementParser = returnStatementParser
    <|> identifierParser

typeRefParser :: Parser Type
typeRefParser = do
    name <- identifier
    return (TypeRef $ RefLocal (Name name))


parser :: Parser Def
parser = do
  model <- modelParser
  return $ Def (Name "UNKNOWN") (ClassDef model)
