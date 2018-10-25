module Interpret.Expression where

import Types
import Interpret.Environment

evaluate :: Environment -> Expression -> IO PValue
evaluate env (Value pval      ) = return pval
evaluate env (Var   identifier) = case fetchVar env identifier of
  Just val -> return val
  Nothing  -> return $ PError ("Variable \"" ++ identifier ++ "\" not bound")
evaluate env (Application expr (PPipeline line)) = undefined
evaluate env (Application expr value) =
  return (PError "Tried to apply a non-pipe value")
