module Interpret.Expression where

import Types
import Interpret.Environment
--import Interpret.Pipe


evaluateExpr :: Environment -> Expression -> IO PValue
evaluateExpr env (Value pval      ) = return pval
evaluateExpr env (Var   identifier) = case fetchVar env identifier of
  Just val -> return val
  Nothing  -> return $ PError ("Variable \"" ++ identifier ++ "\" not bound")
evaluateExpr env (Application (PPipeline line)) = undefined{-do
  value <- evaluateExpr expr
  case value of
    PList list -> PList $ evaluatePipeline env line list
    _ -> undefined-}
evaluateExpr env (Application value) =
  return (PError "Tried to apply a non-pipe value")
