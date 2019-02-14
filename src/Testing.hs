{-# LANGUAGE OverloadedLists #-}
module Testing where

import Types
import Streamly
import Streamly.Prelude
import Interpret.Evaluate

pipe = Anonymous [] (Environment []) ["a","b"] (Var "b")
testEnv = Environment [("a", Def $ Value $ PNumber 42), ("funcLoc", Def $ Value $ PPipe pipe)]
 
list :: ValueStream
list = fromFoldable $ ([PNumber 5, PList $ fromFoldable ([None] :: [PValue]),PBool True] :: [PValue])
