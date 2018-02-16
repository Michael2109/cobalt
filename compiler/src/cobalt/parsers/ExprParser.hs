{-|
Module      : ExprParser
Description : Parses all expressions.
The highest level parser that uses functions in the BaseParser and ABExprParser to generate the AST.
-}
module ExprParser (Parser,
  expr, moduleParser, classParser, parser) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)

import ABExprParser
import Block


assignArith :: Parser Expr
assignArith = do
  var  <- identifier
  symbol ":"
  vType <- valType
  symbol "="
  e <- aExpr
  return $ AssignArith vType var e


stringLiteral :: Parser Expr
stringLiteral = do
  value <- char '"' >> manyTill L.charLiteral (char '"')
  return $ StringLiteral value


assignParser :: String -> Parser Expr
assignParser moduleName = do
  var  <- identifier
  symbol ":"
  vType <- valType
  symbol "="
  e <- expr' moduleName
  return (Assign vType var e)


arrayDef :: Parser Expr
arrayDef = do
  name <- identifier
  symbol ":"
  symbol "["
  arrType <- word
  symbol "]"
  symbol "="
  return $ ArrayDef arrType name


arrayValues :: Parser Expr
arrayValues = do
  symbol "["
  values <- many identifier
  symbol "]"
  return $ ArrayValues values

arrayAssign :: Parser Expr
arrayAssign = do
  def <- arrayDef
  values <- arrayValues
  return $ ArrayAssignment def values

arrayElementSelect :: Parser Expr
arrayElementSelect = do
  symbol "!!"
  elementNum <- word
  return $ ArrayElementSelect elementNum

arrayAppend :: String -> Parser Expr
arrayAppend moduleName = do
  arrays <- sepBy1 (expr' "") (symbol "++")
  return $ ArrayAppend arrays


moduleParser :: [String] -> Parser Expr
moduleParser relativeDir = do
    rword "module"
    name <- identifier
    imports <- many (try importParser)
    exprs <- many (expr' name <|> functionParser name)
    let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
    return (Module (packageDir) name imports exprs)

classParser :: [String] -> Parser Expr
classParser relativeDir = do
    rword "class"
    name <- identifier
    imports <- many (try importParser)
    exprs <- many (expr' name <|> functionParser name)
    let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
    return (Class (packageDir) name imports exprs)

importParser :: Parser Expr
importParser = L.nonIndented scn p
  where
    p = do
      rword "import"
      locations <- sepBy1 identifier (symbol ".")
      return $ (Import locations)

-- Function parser
functionParser :: String -> Parser Expr
functionParser moduleName = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      name <- identifier
      symbol ":"
      argTypes <- some argumentType
      symbol "->"
      rType <- ExprParser.returnType
      nameDup <- L.lineFold scn $ \sp' -> identifier
      args <- many argument
      symbol "="
      if(name == "main")
        then return (L.IndentMany Nothing (return . (MainFunction moduleName name argTypes args rType)) (expr' moduleName))
        else if name == moduleName
          then return (L.IndentMany Nothing (return . (Constructor moduleName name argTypes args)) (expr' ""))
          else return (L.IndentMany Nothing (return . (Function moduleName name argTypes args rType)) (expr' ""))

valType :: Parser Expr
valType = do
    value <- identifier
    return $ Type value

argumentType :: Parser Expr
argumentType = do
    value <- identifier
    return $ ArgumentType value

returnType :: Parser Expr
returnType = do
    value <- identifier
    return $ ReturnType value

argument :: Parser Expr
argument = do
  value <- identifier
  return $ Argument value


functionCallParser :: String -> Parser Expr
functionCallParser moduleName = do
  name <- identifier
  args <- parens $ many argument
  return $ FunctionCall moduleName name args


-- data A = B String | C Integer
dataElementParser :: String -> Parser Expr
dataElementParser superName = do
  name <- identifier
  argTypes <- many identifier
  let args = map (\x -> "var" ++ show x) [0..((length argTypes)-1)]
  return $ DataElement superName name argTypes args


dataParser :: Parser Expr
dataParser = L.nonIndented scn p
  where
    p = do
      rword "data"
      name <- identifier
      symbol "="
      dataElements <- sepBy (dataElementParser name) (symbol "|")
      return $ Data name dataElements


dataInstanceParser :: String -> Parser Expr
dataInstanceParser moduleName = do
  typeName <- valType
  es <- expr' moduleName
  return $ DataInstance moduleName typeName es


printParser :: Parser Expr
printParser = do
  rword "println"
  bodyArr <- identifier
  return $ Print bodyArr


ifStmt :: String -> Parser Expr
ifStmt moduleName = L.indentBlock scn p
   where
     p = do
       rword "if"
       cond  <- bExpr
       return (L.IndentMany Nothing (return . (If cond)) (expr' moduleName))

elseIfStmt :: String -> Parser Expr
elseIfStmt moduleName = L.indentBlock scn p
   where
     p = do
       rword "else"
       rword "if"
       cond  <- bExpr
       return (L.IndentMany Nothing (return . (ElseIf cond)) (expr' moduleName))

elseStmt :: String -> Parser Expr
elseStmt moduleName = L.indentBlock scn p
   where
     p = do
       rword "else"
       return (L.IndentMany Nothing (return . (Else)) (expr' moduleName))

whereStmt :: Parser Expr
whereStmt = do
  rword "where"
  symbol "{"
  exprs <- many expr
  symbol "}"
  return $ (Where exprs)


expr :: Parser Expr
expr = f <$> sepBy1 (expr' "") (symbol ";")
  where
    -- if there's only one expr return it without using ‘Seq’
    f l = if length l == 1 then head l else Seq l


expr' :: String -> Parser Expr
expr' moduleName = try dataParser
  <|> try (functionCallParser moduleName)
  <|> try (ifStmt moduleName)
  <|> try (elseIfStmt moduleName)
  <|> try (elseStmt moduleName)
  <|> try (dataInstanceParser moduleName)
  <|> try arrayAssign
  <|> try arrayElementSelect
  <|> try assignArith
  <|> try (assignParser moduleName)
  <|> try printParser
  <|> try whereStmt
  <|> try stringLiteral


parser :: Parser Expr
parser = expr' ""