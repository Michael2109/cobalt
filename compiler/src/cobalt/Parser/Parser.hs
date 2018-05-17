{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

{-|
Module      : Parser
Description : Parses all expressions.
The highest level parser that uses functions in the BaseParser to generate the AST.
-}
module Parser.Parser where

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
    =   Public    <$ rword "public"
    <|> Protected <$ rword "protected"
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
        expression <- expressionParser'
        if length varNames <= 1
            then return $ Assign (varNames!!0) varType immutable (ExprAssignment expression)
            else return $ AssignMultiple varNames varType immutable (ExprAssignment expression)
    assignDoBlock = L.indentBlock scn p
      where
        p = do
            (immutable, varNames, varType) <- try $ do
                start <- assignStart
                rword "do"
                return start
            if length varNames <= 1
                then return $ L.IndentSome Nothing (return . (Assign (varNames!!0) varType immutable) . StmtAssignment . BlockStmt) statementParser
                else return $ L.IndentSome Nothing (return . (AssignMultiple varNames varType immutable) . StmtAssignment . BlockStmt) statementParser
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

caseParser :: Parser Case
caseParser = do
    kase <- choice [caseDoBlock, caseInline]
    return kase
  where
    caseInline = do
        id <- caseStart
        expression <- expressionParser'
        return $ Case id $ ExprAssignment expression
    caseDoBlock = L.indentBlock scn p
      where
        p = do
            id <- try $ do
                start <- caseStart
                rword "do"
                return start
            return (L.IndentSome Nothing (return . (Case id) . StmtAssignment . BlockStmt) statementParser)
    caseStart = do
        id <- try $ do
            identifier
            symbol "->"
            return id
        return id

expressionParser :: Parser Expr
expressionParser
    =   newClassInstanceParser
    <|> tupleParser
    <|> parens expressionParser'
    <|> methodCallParser
    <|> ternaryParser
    <|> IntConst <$> integerParser
    <|> (BoolConst True  <$ rword "True")
    <|> (BoolConst False <$ rword "False")
    <|> specialRefAsExprParser
    <|> identifierParser
    <|> stringLiteralParser

expressionParser' :: Parser Expr
expressionParser'
    =   makeExprParser nestedExpressionParser operators

expressionAsStatementParser :: Parser Stmt
expressionAsStatementParser = ExprAsStmt <$> expressionParser'

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
      start <- expressionParser'
      rword "to"
      end <- expressionParser
      symbol ")"
      return (L.IndentMany Nothing (return . (For varName start end) . BlockStmt) statementParser)

identifierParser :: Parser Expr
identifierParser = do
    name <- nameParser
    return $ Identifier name

ifStatementParser :: Parser Stmt
ifStatementParser  = do
    (condition, ifStatements) <- ifParser
    elseSection <- elseParser
    return $ If condition ifStatements elseSection
  where
    controlParser = do
        condition <- expressionParser'
        rword "then"
        return condition

    ifParser = do
        try $ L.indentBlock scn p
          where
            p = do
                rword "if"
                condition <- controlParser
                return (L.IndentSome Nothing (return .  (condition, ) . BlockStmt) statementParser)

    elseParser = do
        elifBlock <- optional elifP
        case elifBlock of
            Just a -> do
                ee <- elseParser
                return (Just (If (fst a) (snd a) ee))
            Nothing -> do
                optional elseP
      where
        elifP = do
            try $ L.indentBlock scn p
              where
                  p = do
                      rword "elif"
                      condition <- controlParser
                      return (L.IndentSome Nothing (return . (condition, ) . BlockStmt) statementParser)
        elseP = do
            try $ L.indentBlock scn p
              where
                p = do
                    rword "else"
                    return (L.IndentSome Nothing (return . BlockStmt) statementParser)

importParser :: Parser Import
importParser = try $ L.nonIndented scn p
  where
    p = do
        try (rword "import")
        locations <- sepBy1 identifier (symbol ".")
        return $ (Import locations)

inlineParser :: Parser [Stmt]
inlineParser = sepBy statementParser (symbol ";")

lambdaParser :: Parser Stmt
lambdaParser = do
    lambda <- choice [lambdaDoBlock, lambdaInline]
    return lambda
  where
    lambdaInline = do
        fields <- lambdaStart
        expression <- expressionParser'
        return $ Lambda fields $ ExprAssignment expression
    lambdaDoBlock = L.indentBlock scn p
      where
        p = do
            fields <- try $ do
                start <- lambdaStart
                rword "do"
                return start
            return (L.IndentSome Nothing (return . (Lambda fields) . StmtAssignment . BlockStmt) statementParser)
    lambdaStart = do
        fields <- try $ do
            rword "fun"
            fields <- choice [parens (sepBy fieldParser (symbol ",")), sepBy fieldParser (symbol ",")]
            symbol "->"
            return fields
        return fields

methodParser :: Parser Method
methodParser = do
    method <- choice [methodDoBlock, methodInline]
    return method
  where
    methodInline = do
        (annotations, modifiers, name, fields, returnType) <- methodStart
        expression <- expressionParser'
        return $ Method name annotations fields modifiers returnType $ ExprAssignment expression
    methodDoBlock = L.indentBlock scn p
      where
        p = do
            (annotations, modifiers, name, fields, returnType) <- try $ do
                start <- methodStart
                rword "do"
                return start
            return (L.IndentSome Nothing (return . (Method name annotations fields modifiers returnType) . StmtAssignment . BlockStmt) statementParser)
    methodStart = do
        (annotations, modifiers, name, fields) <- try $ do
          annotations <- many annotationParser
          modifiers <- modifiersParser
          rword "let"
          name <- choice [Name <$> rword "this", nameParser]
          fields <- parens $ sepBy fieldParser $ symbol ","
          return (annotations, modifiers, name, fields)
        returnType <- optional $ do
            symbol ":"
            typeRefParser
        symbol "="
        return (annotations, modifiers, name, fields, if name == Name "this" then Just Init else returnType)

methodCallParser :: Parser Expr
methodCallParser =
    try $ do
        methodName <- nameParser
        args <- parens $ sepBy expressionParser' (symbol ",")
        return $ MethodCall methodName (BlockExpr args)

methodDefParser :: Parser Stmt
methodDefParser = MethodDef <$> methodParser

modelParser :: Parser Model
modelParser = L.indentBlock scn p
  where
    p = do
        (modifiers, modelType) <- try $ do
            modifiers <- modifiersParser
            modelType <- modelTypeParser
            return (modifiers, modelType)
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
        return (L.IndentMany Nothing (return . (Model (Name name) modelType modifiers fields parent parentArguments interfaces) . BlockStmt) statementParser)

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

nestedExpressionParser :: Parser Expr
nestedExpressionParser = do
    expressions <- sepBy1 expressionParser (symbol ".")
    if length expressions == 1
    then return $ expressions!!0
    else return $ BlockExpr expressions

newClassInstanceParser :: Parser Expr
newClassInstanceParser = do
    choice [newClassInstanceAnonymousClass, newClassInstance]
  where
    newClassInstance = do
        (className, arguments) <- newClassInstanceStart
        return $ (NewClassInstance className (BlockExpr arguments) Nothing)
    newClassInstanceAnonymousClass = try $ L.indentBlock scn p
      where
        p = do
            (className, arguments) <- newClassInstanceStart
            return (L.IndentSome Nothing (return . (NewClassInstance className (BlockExpr arguments)) . Just . BlockStmt) statementParser)
    newClassInstanceStart = do
        try (rword "new")
        className <- typeRefParser
        arguments <- parens $ sepBy expressionParser' (symbol ",")
        return (className, arguments)

operators :: [[Operator Parser Expr]]
operators =
    [   [ Prefix (Neg <$ symbol "-")
        , Prefix (Not <$ rword  "not")
        ]
    ,   [ InfixL (Array ArrayAppend <$ rword "++")
        ]
    ,   [ InfixL (ABinary Multiply <$ symbol "*")
        , InfixL (ABinary Divide   <$ symbol "/")
        ]
    ,   [ InfixL (ABinary Add      <$ symbol "+")
        , InfixL (ABinary Subtract <$ symbol "-")
        ]
    ,   [ InfixL (RBinary GreaterEqual <$ rword ">=")
        , InfixL (RBinary LessEqual    <$ rword "<=")
        , InfixL (RBinary Greater      <$ rword ">" )
        , InfixL (RBinary Less         <$ rword "<" )
        ]
    ,   [ InfixL (BBinary And <$ rword "and")
        , InfixL (BBinary Or  <$ rword "or")
        ]
    ]

reassignParser :: Parser Stmt
reassignParser = do
    name <- try $ do
        id <- identifier
        symbol "<-"
        return $ Name id
    value <- expressionParser'
    return $ Reassign name $ ExprAssignment value

relation :: Parser RBinOp
relation
  =   (symbol ">=" *> pure GreaterEqual)
  <|> (symbol "<=" *> pure LessEqual)
  <|> (symbol ">" *> pure Greater)
  <|> (symbol "<" *> pure Less)

returnStatementParser :: Parser Stmt
returnStatementParser = do
    try $ rword "return"
    statement <- statementParser
    return $ Return statement

statementParser :: Parser Stmt
statementParser = modelDefParser
    <|> methodDefParser
    <|> returnStatementParser
    <|> ifStatementParser
    <|> lambdaParser
    <|> assignParser
    <|> reassignParser
    <|> forLoopGeneratorParser
    <|> expressionAsStatementParser

statementBlockParser :: Parser Stmt
statementBlockParser = BlockStmt <$> some statementParser

stringLiteralParser :: Parser Expr
stringLiteralParser = do
    value <- char '"' >> manyTill r (symbol "\"")
    return $ StringLiteral value
  where
    r = label "Unexpected end of line in single line string literal" $ do
            notFollowedBy (char '\n')
            L.charLiteral

stringLiteralMultilineParser :: Parser Expr
stringLiteralMultilineParser = do
    symbol "```"
    contents <- many $ L.lineFold scn $ \sp' -> some L.charLiteral
    symbol "```"
    return $ StringLiteral $ intercalate "\n" contents

superParser :: Parser SpecialRef
superParser = Super <$ rword "super"

ternaryParser :: Parser Expr
ternaryParser  = do
    try $ rword "if"
    condition  <- expressionParser'
    rword "then"
    ifExpression <- expressionParser'
    rword "else"
    elseExpression <- expressionParser'
    return $ Ternary condition ifExpression elseExpression

specialRefAsExprParser :: Parser Expr
specialRefAsExprParser
    =   SpecialRefAsExpr This <$ rword "this"
    <|> SpecialRefAsExpr Super <$ rword "super"

tryBlockParser :: Parser Stmt
tryBlockParser  = do
    trySection   <- L.indentBlock scn tryP
    catchSection <- optional $ L.indentBlock scn catchP
    finallySection <- optional $ L.indentBlock scn finallyP
    return $ TryBlock trySection catchSection finallySection
  where
    tryP = do
      rword "try"
      return (L.IndentMany Nothing (return . (TryStatement) . BlockStmt) statementParser)
    catchP = do
      rword "catch"
      fields <- parens $ sepBy fieldParser $ symbol ","
      return (L.IndentMany Nothing (return . (CatchStatement fields) . BlockStmt) statementParser)
    finallyP = do
      rword "finally"
      return (L.IndentMany Nothing (return . (FinallyStatement) . BlockStmt) statementParser)

tupleParser :: Parser Expr
tupleParser =
    try $ do
        lookAhead $ do
            symbol "("
            expressionParser'
            symbol ","
        values <- try $ parens $ sepBy1 expressionParser' (symbol ",")
        return $ Tuple (BlockExpr values)

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
        condition <- parens expressionParser'
        return (L.IndentMany Nothing (return . (While condition) . BlockStmt) statementParser)

parser :: Parser Module
parser = do
    nameSpace <- nameSpaceParser
    imports <- many importParser
    models <- many modelParser
    eof
    return $ Module (ModuleHeader nameSpace imports) models
