{-# LANGUAGE Rank2Types #-}
module Interpret.Environment where

import Types
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens

fetchVar :: Environment -> Identifier -> Maybe PValue
fetchVar = fetch vars

writeVar :: Environment -> Identifier -> PValue -> Environment
writeVar = write vars

fetchDef :: Environment -> Identifier -> Maybe Definition
fetchDef = fetch defs

writeDef :: Environment -> Identifier -> Definition -> Environment
writeDef = write defs

fetch :: Lens' Environment (Map Identifier a) -> Environment -> Identifier -> Maybe a
fetch l env identifier = env ^. l & Map.lookup identifier

write :: Lens' Environment (Map Identifier a) -> Environment -> Identifier -> a -> Environment
write l env identifier val = env & l %~ Map.insert identifier val
