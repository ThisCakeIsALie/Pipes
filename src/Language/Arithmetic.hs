{-# LANGUAGE OverloadedLists #-}
module Language.Arithmetic where

import Types
import Prelude hiding (succ, pred)
import Language.Native

add :: Value -> Value -> Runtime Value
add (Number a) (Number b) = pure $ Number $ a + b

pred :: Value -> Runtime Value
pred (Number a) = pure $ Number (a - 1)

succ :: Value -> Runtime Value
succ (Number a) = pure $ Number (a + 1)

arithmetic :: Environment
arithmetic = [
        globalPipe "+" add,
        globalPipe "pred" pred,
        globalPipe "succ" succ
    ]