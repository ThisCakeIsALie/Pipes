{-# LANGUAGE Rank2Types #-}
module Interpret.Environment where

import Types
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens

fetchDef :: Environment -> Identifier -> Maybe Definition
fetchDef env identifier = env ^. defs & Map.lookup identifier
