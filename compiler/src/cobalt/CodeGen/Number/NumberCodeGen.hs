module CodeGen.Number.NumberCodeGen where

import JVM.ClassFile
import JVM.Builder

intToBytecode value
    | value == 0 = iconst_0
    | value == 1 = iconst_1
    | value == 2 = iconst_2
    | value == 3 = iconst_3
    | value == 4 = iconst_4
    | value == 5 = iconst_5
    | value >= -128 && value <= 127 = bipush $ fromIntegral value
    | value >= -32768 && value <= 32767 = sipush $ fromIntegral value
    | otherwise = ldc2 $ CInteger $ fromIntegral value

longToBytecode value
    | value == 0 = lconst_0
    | value == 1 = lconst_1
    | value >= -128 && value <= 127 = bipush $ fromIntegral value
    | value >= -32768 && value <= 32767 = sipush $ fromIntegral value
    | otherwise = ldc2w $ CLong $ fromIntegral value

floatToBytecode value
    | value == 0 = fconst_0
    | value == 1 = fconst_1
    | value == 2 = fconst_2
    | otherwise = ldc2 $ CFloat $ realToFrac value

doubleToBytecode value
    | value == 0 = dconst_0
    | value == 1 = dconst_1
    | otherwise = ldc2w $ CDouble value
