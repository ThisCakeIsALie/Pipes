{-# LANGUAGE OverloadedLists #-}
module Testing where

import Types
import Streamly
import Streamly.Prelude

pipe = Pipe [] ["a","b"] [Var "a", Value $ PList $ fromFoldable ([PNumber 3, PBool True] :: [PValue])]
pipeCall = Local pipe []
testEnv = Environment [("a",PNumber 42),("funcLoc", PPipeline (Connect pipeCall End))] []

list :: PValue
list = PList $ fromFoldable $ ([PNumber 5, PList $ fromFoldable ([None] :: [PValue]),PBool True] :: [PValue])
