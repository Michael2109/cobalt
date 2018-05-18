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
        newMethod [ACC_PUBLIC] (pack $ extractName methodName) [] ReturnsVoid (return ())
        return ()

instance CodeGen AST.Stmt where
    genCode (AST.BlockStmt statements) = forM_ statements genCode
    genCode (AST.MethodDef method) = genCode method
    genCode (_) = return ()

extractName (AST.Name value) = value
