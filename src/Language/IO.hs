{-# LANGUAGE OverloadedLists #-}
module Language.IO where

import Types
import Language.Presets
import Data.Map (Map)
import System.IO

ioBuiltins :: Map Identifier Definition
ioBuiltins = [("println", asPipe printlnValue),("print", asPipe printValue),("input", asPipe inputValue)]

printlnValue :: PValue -> IO PValue
printlnValue val = print val >> return val

printValue :: PValue -> IO PValue
printValue val = putStr (show val) >> hFlush stdout >> return val

inputValue :: IO PValue
inputValue = PString <$> getLine