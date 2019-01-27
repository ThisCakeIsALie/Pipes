{-# LANGUAGE OverloadedLists #-}
module Testing where

import Types
import Streamly
import Streamly.Prelude
import Interpret.Evaluate

pipe = Pipe [] ["a","b"] [Var "a" [], Var "b" []]
testEnv = Environment [("a",asDefinition $ PNumber 42), ("funcLoc", asDefinition $ PPipe pipe)]
 
list :: ValueStream
list = fromFoldable $ ([PNumber 5, PList $ fromFoldable ([None] :: [PValue]),PBool True] :: [PValue])
