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
import Parser.BaseParser
import Parser.ParserType
import SymbolTable.SymbolTable

abstractModifierParser :: Parser Modifier
abstractModifierParser = Abstract <$ rword "abstract"

accessModifierParser :: Parser Modifier
accessModifierParser
    =   Public    <$ rword "member"
    <|> Protected <$ rword "protected"
    <|> Private   <$ rword "private"
    <|> PackageLocal <$ rword "local"

annotationParser :: Parser Annotation
annotationParser = do
    symbol "@"
    name <- identifier
    return $ Annotation $ Name name

assignParser :: Parser Stmt
assignParser = do
    assign <- choice [assignDoBlock, assignInline]
    return assign
  where
    assignInline = do
        (immutable, varNames, varType) <- try $ do
            start <- assignStart
            return start
        statements <- some statementParser
        if length varNames <= 1
            then return $ Assign (varNames!!0) varType (Block statements)
            else return $ AssignMultiple varNames varType (Block statements)
    assignDoBlock = L.indentBlock scn p
      where
        p = do
            (immutable, varNames, varType) <- try $ do
                start <- assignStart
                rword "do"
                return start
            if length varNames <= 1
                then return $ L.IndentSome Nothing (return . (Assign (varNames!!0) varType) . Block) statementParser
                else return $ L.IndentSome Nothing (return . (AssignMultiple varNames varType) . Block) statementParser
    assignStart = do
        start <- try $ do
            rword "let"
            mutableOpt <- optional $ rword "mutable"
            let immutable = case mutableOpt of
                                Nothing -> True
                                Just _  -> False
            varNames <- sepBy nameParser (symbol ",")
            varType <- optional $ do
                symbol ":"
                vType <- typeRefParser
                return vType
            symbol "="
            return (immutable, varNames, varType)
        return start

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
    [ [Prefix (Neg <$ symbol "-") ]
    , [ InfixL (ABinary Multiply <$ symbol "*")
      , InfixL (ABinary Divide   <$ symbol "/") ]
    , [ InfixL (ABinary Add      <$ symbol "+")
      , InfixL (ABinary Subtract <$ symbol "-") ]
    ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
    [ [Prefix (Not <$ rword "not") ]
    , [InfixL (BBinary And <$ rword "and")
      , InfixL (BBinary Or <$ rword "or") ]
    ]

aTerm :: Parser AExpr
aTerm
    =   parens aExpr
    <|> Var      <$> identifier
    <|> IntConst <$> integerParser
    <|> IntConst <$> integerParser
    <|> IntConst <$> integerParser
    <|> IntConst <$> integerParser

bTerm :: Parser BExpr
bTerm =  parens bExpr
  <|> (BoolConst True  <$ rword "True")
  <|> (BoolConst False <$ rword "False")
  <|> rExpr

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)

relation :: Parser RBinOp
relation = (symbol ">" *> pure Greater)
  <|> (symbol "<" *> pure Less)

expressionParser :: Parser Expr
expressionParser
    = Block <$> many statementParser

fieldParser :: Parser Field
fieldParser = do
    name <- identifier
    varType <- optional $ do
        symbol ":"
        varType <- typeRefParser
        return varType
    return $ Field (Name name) varType Nothing

finalModifierParser :: Parser Modifier
finalModifierParser = Final <$ rword "final"

forLoopGeneratorParser :: Parser Stmt
forLoopGeneratorParser  = try $ L.indentBlock scn p
  where
    p = do
      rword "for"
      symbol "("
      varName <- identifierParser
      symbol "<-"
      start <- aTerm
      rword "to"
      end <- aTerm
      symbol ")"
      return (L.IndentMany Nothing (return . (For varName start end) . Block) statementParser)

identifierParser :: Parser Stmt
identifierParser = do
    name <- nameParser
    return $ Identifier name

ifStatementParser :: Parser Stmt
ifStatementParser  = do
    ifSection   <- L.indentBlock scn ifP
    elifSection <- optional $ L.indentBlock scn elifP
    elseSection <- optional $ L.indentBlock scn elseP
    return $ If ifSection elifSection elseSection
  where
    ifP = do
      rword "if"
      condition  <- parens bExpr
      return (L.IndentMany Nothing (return . (IfStatement condition) . Block) statementParser)
    elifP = do
      rword "elif"
      condition  <- parens bExpr
      return (L.IndentMany Nothing (return . (ElifStatement condition) . Block) statementParser)
    elseP = do
      rword "else"
      return (L.IndentMany Nothing (return . (ElseStatement) . Block) statementParser)

importParser :: Parser Import
importParser = try $ L.nonIndented scn p
  where
    p = do
        try (rword "import")
        locations <- sepBy1 identifier (symbol ".")
        return $ (Import locations)

inlineExpressionParser :: Parser Expr
inlineExpressionParser = f <$> sepBy1 (statementParser) (symbol ";")
  where
    -- if there's only one expr return it without using ‘Seq’
    f l = if length l == 1 then Block [head l] else Block l

lambdaParser :: Parser Stmt
lambdaParser = do
    lambda <- choice [lambdaDoBlock, lambdaInline]
    return lambda
  where
    lambdaInline = do
        fields <- lambdaStart
        statements <- some statementParser
        return $ Lambda fields (Block statements)
    lambdaDoBlock = L.indentBlock scn p
      where
        p = do
            fields <- try $ do
                start <- lambdaStart
                rword "do"
                return start
            return (L.IndentSome Nothing (return . (Lambda fields) . Block) statementParser)
    lambdaStart = do
        fields <- try $ do
            rword "fun"
            fields <- many $ choice [parens fieldParser, fieldParser]
            symbol "->"
            return fields
        return fields

methodParser :: Parser Method
methodParser = L.indentBlock scn p
  where
    p = do
      (annotations, modifiers) <- try $ do
          anns <- many annotationParser
          mods <- modifiersParser
          rword "let"
          return (anns, mods)
      name <- identifier
      fields <- parens $ sepBy fieldParser $ symbol ","
      symbol ":"
      returnType <- typeRefParser
      return (L.IndentMany Nothing (return . (Method (Name name) annotations fields modifiers returnType) . Block) statementParser)

methodCallParser :: Parser Stmt
methodCallParser =
    try $ do
        methodName <- nameParser
        args <- parens $ sepBy statementParser (symbol ",")
        return $ MethodCall methodName (Block args)

methodDefParser :: Parser Stmt
methodDefParser = MethodDef <$> methodParser

modelParser :: Parser Model
modelParser = L.indentBlock scn p
  where
    p = do
        modifiers <- try $ do
            mods <- modifiersParser
            rword "class"
            return mods
        name <- identifier
        fieldsOpt <- optional $ parens $ sepBy fieldParser (symbol ",")
        let fields = case fieldsOpt of
                             Just fs -> fs
                             Nothing -> []
        extendsKeyword <- optional $ rword "extends"
        parent <- optional typeRefParser
        parentArgumentsOpt <- optional $ parens $ sepBy statementParser (symbol ",")
        let parentArguments = case parentArgumentsOpt of
                             Just fs -> fs
                             Nothing -> []
        implementsKeyword <- optional $ rword "implements"
        interfaces <- sepBy typeRefParser (symbol ",")
        return (L.IndentMany Nothing (return . (Model (Name name) modifiers fields parent parentArguments interfaces) . Block) statementParser)

modelDefParser :: Parser Stmt
modelDefParser = ModelDef <$> modelParser

modelTypeParser :: Parser ModelType
modelTypeParser
    =   ClassModel    <$ rword "class"
    <|> ObjectModel   <$ rword "object"
    <|> TraitModel    <$ rword "trait"

modifiersParser :: Parser [Modifier]
modifiersParser = many $ choice [accessModifierParser, abstractModifierParser, finalModifierParser]

nameParser :: Parser Name
nameParser = do
    id <- identifier
    return $ Name id

nameSpaceParser :: Parser NameSpace
nameSpaceParser = try $ L.nonIndented scn p
  where
    p = do
        try (rword "package")
        locations <- sepBy1 identifier (symbol ".")
        return $ (NameSpace locations)

newClassInstanceParser :: Parser Stmt
newClassInstanceParser  = do
    try (rword "new")
    className <- typeRefParser
    arguments <- parens $ sepBy statementParser (symbol ",")
    return $ (NewClassInstance className arguments)

reassignParser :: Parser Stmt
reassignParser = do
    name <- try $ do
        id <- identifier
        symbol "<-"
        return (Name id)
    value <- expressionParser
    return (Reassign name value)

returnStatementParser :: Parser Stmt
returnStatementParser = do
    try $ rword "return"
    statement <- statementParser
    return $ Return statement

statementParser :: Parser Stmt
statementParser = modelDefParser
    <|> methodDefParser
    <|> returnStatementParser
    <|> identifierParser

stringLiteralParser :: Parser Stmt
stringLiteralParser = do
    value <- char '"' >> manyTill L.charLiteral (char '"')
    return $ StringLiteral value

stringLiteralMultilineParser :: Parser Stmt
stringLiteralMultilineParser = do
    symbol "```"
    contents <- many $ L.lineFold scn $ \sp' -> some L.charLiteral
    symbol "```"
    return $ StringLiteral $ intercalate "\n" contents

superParser :: Parser SpecialRef
superParser = Super <$ rword "super"

thisParser :: Parser SpecialRef
thisParser = This <$ rword "this"

tryBlockParser :: Parser Stmt
tryBlockParser  = do
    trySection   <- L.indentBlock scn tryP
    catchSection <- optional $ L.indentBlock scn catchP
    finallySection <- optional $ L.indentBlock scn finallyP
    return $ TryBlock trySection catchSection finallySection
  where
    tryP = do
      rword "try"
      return (L.IndentMany Nothing (return . (TryStatement) . Block) statementParser)
    catchP = do
      rword "catch"
      fields <- parens $ sepBy fieldParser $ symbol ","
      return (L.IndentMany Nothing (return . (CatchStatement fields) . Block) statementParser)
    finallyP = do
      rword "finally"
      return (L.IndentMany Nothing (return . (FinallyStatement) . Block) statementParser)

tupleParser :: Parser Stmt
tupleParser =
    try $ do
        values <- parens $ sepBy identifierParser (symbol ",")
        return $ Tuple values

typeParameterParser :: Parser [Type]
typeParameterParser = do
    try $ symbol "["
    types <- sepBy typeRefParser (symbol ",")
    symbol "]"
    return types

typeRefParser :: Parser Type
typeRefParser = do
    name <- identifier
    return (TypeRef $ RefLocal (Name name))

whileParser :: Parser Stmt
whileParser  = try $ L.indentBlock scn p
  where
    p = do
        rword "while"
        condition <- parens bTerm
        return (L.IndentMany Nothing (return . (While condition) . Block) statementParser)

parser :: Parser Module
parser = do
    nameSpace <- nameSpaceParser
    imports <- many importParser
    models <- many modelParser
    return $ Module (ModuleHeader nameSpace imports) models
