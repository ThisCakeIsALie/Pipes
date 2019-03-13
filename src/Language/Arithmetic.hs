{-# LANGUAGE OverloadedLists #-}
module Language.Arithmetic where

import Types
import Language.Presets
import Data.Map (Map)
import Data.Fixed

arithmeticBuiltins :: Map Identifier Definition
arithmeticBuiltins = [
        ("succ", asPipe succValue),
        ("+", asPipe addValue),
        ("-", asPipe subtractValue),
        ("*", asPipe multiplyValue),
        ("/", asPipe divideValue),
        ("mod", asPipe modValue),
        ("pi", PNumber pi)
    ]

succValue :: PValue -> IO PValue
succValue (PNumber n) = return $ PNumber (n + 1)

addValue :: PValue -> PValue -> IO PValue
addValue (PNumber x) (PNumber y) = return $ PNumber (x + y)

subtractValue :: PValue -> PValue -> IO PValue
subtractValue (PNumber x) (PNumber y) = return $ PNumber (x - y)

multiplyValue :: PValue -> PValue -> IO PValue
multiplyValue (PNumber x) (PNumber y) = return $ PNumber (x * y)

divideValue :: PValue -> PValue -> IO PValue
divideValue (PNumber x) (PNumber y) = return $ PNumber (x / y)

modValue :: PValue -> PValue -> IO PValue
modValue (PNumber x) (PNumber y) = return $ PNumber (x `mod'` y)