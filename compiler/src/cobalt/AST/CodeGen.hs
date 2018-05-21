{-# LANGUAGE FlexibleContexts #-}

module AST.CodeGen where

import Data.Char
import Data.List
import Data.Maybe
import Text.Format
import Text.PrettyPrint.Annotated.Leijen
import Control.Monad
import Control.Monad.Exception
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack)
import Data.Traversable

import qualified AST.AST as AST
import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Builder
import JVM.Exceptions
import qualified Java.Lang
import qualified Java.IO
import SymbolTable.SymbolTable
import Util.GeneralUtil

class CodeGen a where
    genCode :: Throws UnexpectedEndMethod e => a -> Generate e ()

instance CodeGen AST.Module where
    genCode (AST.Module header modules) = forM_ modules genCode

instance CodeGen AST.Model where
    genCode (AST.Model modelName modelType modelModifiers modelFields modelParent modelParentArguments modelInterfaces modelBody) = genCode modelBody

instance CodeGen AST.Method where
    genCode (AST.Method methodName methodAnns methodParams methodModifiers methodReturnType methodBody) = do

        let convertModifier m = case m of
                                        AST.Public        -> ACC_PUBLIC
                                        AST.Protected     -> ACC_PROTECTED
                                        AST.Abstract      -> ACC_ABSTRACT
                                        AST.Final         -> ACC_FINAL
                                        (_)               -> ACC_PRIVATE

        let modifiers = map convertModifier methodModifiers

        newMethod modifiers (pack $ extractName methodName) [] ReturnsVoid (genCode methodBody)
        return ()

instance CodeGen AST.Block where
    genCode (AST.Inline expression) = genCode expression
    genCode (AST.DoBlock statement) = genCode statement

instance CodeGen AST.Expr where
    genCode (AST.BlockExpr expressions) = forM_ expressions genCode
    genCode (AST.IntConst value)
        | value == 0 = iconst_0
        | value == 1 = iconst_1
        | value == 2 = iconst_2
        | value == 3 = iconst_3
        | value == 4 = iconst_4
        | value == 5 = iconst_5
        | value >= -128 && value <= 127 = bipush $ fromIntegral value
        | value >= -32768 && value <= 32767 = sipush $ fromIntegral value
        | otherwise = return ()

instance CodeGen AST.Stmt where
    genCode (AST.BlockStmt statements) = forM_ statements genCode
    genCode (AST.MethodDef method) = genCode method
    genCode (AST.ExprAsStmt expression) = genCode expression
    genCode (_) = return ()

extractName (AST.Name value) = value
