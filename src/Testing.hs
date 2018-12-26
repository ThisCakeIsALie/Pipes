{-# LANGUAGE OverloadedLists #-}
module Testing where

import Types
import Streamly
import Streamly.Prelude

pipe = Pipe [] ["a","b"] [Var "a", Value $ PList $ fromFoldable ([PNumber 3, PBool True] :: [PValue])]
pipeCall = Local pipe [Value $ PNumber 42, Value $ PNumber 1337]
testEnv = Environment [("a",PNumber 42),("funcLoc", PPipeline (Connect pipeCall End))] []

list :: ValueStream
list = fromFoldable $ ([PNumber 5, PList $ fromFoldable ([None] :: [PValue]),PBool True] :: [PValue])
