{-# LANGUAGE OverloadedLists #-}
module Testing where

import Types
import Streamly
import Streamly.Prelude

pipe = Anonymous [In "a", In "b", Out $ Var "a", Out $ Value $ PList $ fromFoldable ([PNumber 3, PBool True] :: [PValue])]
testEnv = Environment [("a",PNumber 42),("funcLoc", PPipeline (Connect pipe End))] []

list :: SerialT IO PValue
list = fromFoldable $ ([PNumber 5, PList $ fromFoldable ([None] :: [PValue]),PBool True] :: [PValue])