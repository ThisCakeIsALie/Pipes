{-# LANGUAGE Rank2Types #-}
module Interpret.Environment where

import Types
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens

fetchDef :: Environment -> Identifier -> Maybe Definition
fetchDef env identifier = env ^. defs & Map.lookup identifier

updateEnvWithValue :: (Identifier, PValue) -> Environment -> Environment
updateEnvWithValue (identifier, value) env = env & defs %~ Map.insert identifier (Def (Value value))

localizeEnvWithValue :: [(Identifier, PValue)] -> Environment -> Environment
localizeEnvWithValue toUpdate env = foldr updateEnvWithValue env toUpdate
 