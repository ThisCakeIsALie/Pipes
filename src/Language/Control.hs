{-# LANGUAGE OverloadedLists #-}
module Language.Control where

import Types
import Prelude hiding (pred,map)
import Language.Native
import Interpret.Evaluate

apply :: Environment -> Value -> Value -> Runtime Value
apply env f x = applyValue env f [x]

map :: Environment -> Value -> Value -> Runtime Value
map env f x = undefined

control :: Environment
control = [
    globalPipe "apply" apply,
    globalPipe "map" map
  ]