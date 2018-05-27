{-# LANGUAGE OverloadedStrings #-}
-- | This module exports definitions for some most used classes and methods from standard Java java.io package.
module Java.IO where

import Data.String

import JVM.Common ()  -- import instances only
import JVM.ClassFile

import qualified Java.Lang

-- | java.io.PrintStream class name
printStream :: IsString s => s
printStream = "java/io/PrintStream"

-- | java.io.PrintStream class as field type
printStreamClass ::  FieldType
printStreamClass = ObjectType printStream

print :: MethodSignature -> NameType (Method Direct)
print methodSignature = NameType "print" methodSignature

println :: MethodSignature -> NameType (Method Direct)
println methodSignature= NameType "println" methodSignature

out :: NameType (Field Direct)
out = NameType "out" printStreamClass

printf :: MethodSignature ->  NameType (Method Direct)
printf methodSignature = NameType "printf" methodSignature
