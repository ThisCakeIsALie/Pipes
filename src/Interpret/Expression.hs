module Interpret.Expression where

import Types
import Interpret.Environment

evaluateExpr :: Environment -> Expression -> IO PValue
evaluateExpr env (Value pval      ) = return pval
evaluateExpr env (Var   identifier) = case fetchVar env identifier of
  Just val -> return val
  Nothing  -> return $ PError ("Variable \"" ++ identifier ++ "\" not bound")
evaluateExpr env (Application expr (PPipeline line)) = undefined
evaluateExpr env (Application expr value) =
  return (PError "Tried to apply a non-pipe value")
