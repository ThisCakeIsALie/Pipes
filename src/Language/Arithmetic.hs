{-# LANGUAGE OverloadedLists #-}
module Language.Arithmetic where

import Types
import Language.Presets
import Data.Map (Map)

arithmeticBuiltins :: Map Identifier Definition
arithmeticBuiltins = [("succ", Def $ Value $ unaryBuiltin succValue)]

succValue :: PValue -> IO PValue
succValue (PNumber n) = return $ PNumber (n + 1)