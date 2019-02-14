{-# LANGUAGE OverloadedLists #-}
module Language.Control where

import Types
import Language.Presets
import Data.Map (Map)

controlBuiltins :: Map Identifier Definition
controlBuiltins = [("~", Def $ Value $ unaryBuiltin idValue)]

idValue :: PValue -> IO PValue
idValue = return