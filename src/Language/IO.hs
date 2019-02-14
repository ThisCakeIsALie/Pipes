{-# LANGUAGE OverloadedLists #-}
module Language.IO where

import Types
import Language.Presets
import Data.Map (Map)

ioBuiltins :: Map Identifier Definition
ioBuiltins = [("print", Def $ Value $ unaryBuiltin printValue)]

printValue :: PValue -> IO PValue
printValue val = do
    print val
    return None