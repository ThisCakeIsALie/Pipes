{-# LANGUAGE OverloadedLists #-}
module Testing where

import Types
import Streamly
import Streamly.Prelude
import Interpret.Evaluate

pipe = Anonymous [] [] ["a","b"] (Var "b")

testEnv :: Environment
testEnv = [("a", PNumber 42), ("funcLoc", PPipe pipe)]
 
list :: ValueStream
list = fromFoldable $ ([PNumber 5, PList $ fromFoldable ([None] :: [PValue]),PBool True] :: [PValue])
