module Interpret.Pipe where

import Streamly
import Streamly.Prelude as S
import Types
import Interpret.Environment
import Control.Monad.Trans

evaluatePipe :: Environment -> Pipe -> Runtime PValue -> Runtime PValue
evaluatePipe env (Call identifier args) s = case fetchVar env identifier of
  Just (PPipeline line) -> evaluatePipeline env line $ s --TODO: fetch from Defs as well and update s for recursive call
  Just _ -> lift . S.once . return $ PError "Tried to apply a non-pipe value"
  _ -> lift . S.once . return $ PError ("Variable \"" ++ identifier ++ "\" not bound")
evaluatePipe env (Anonymous comp) s = undefined

evaluatePipeline
  :: Environment -> Pipeline -> Runtime PValue -> Runtime PValue
evaluatePipeline env _ = undefined
