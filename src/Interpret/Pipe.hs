{-# LANGUAGE FlexibleContexts #-}
module Interpret.Pipe where

import Streamly
import qualified Streamly.Prelude as S
import Types
import Interpret.Environment
import Control.Monad.Trans
import Control.Monad.State
import Control.Lens
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Interpret.Expression

evaluatePipe :: Environment -> Pipe -> ValueStream -> ValueStream
evaluatePipe env (Call identifier args) s = undefined
{-
  s
    >>  get
    >>= (\env -> case fetchVar env identifier of
          Just (PPipeline line) -> evaluatePipeline env line $ s --TODO: fetch from Defs as well and update s for recursive call
          Just _ ->
            lift . S.once . return $ PError "Tried to apply a non-pipe value"
          _ -> lift . S.once . return $ PError
            ("Variable \"" ++ identifier ++ "\" not bound")
        )
 -}
evaluatePipe env (Anonymous comp) s =
  let inputs  = mapMaybe (^? _In) comp
      inputSize = length inputs
      outputs = mapMaybe (^? _Out) comp
  in applyMultiple inputSize (mapper inputs outputs) s 
  --let localEnv = foldr updateScope env (zip inputs values)
  where
    updateEnv (identifier, value) env = env & vars %~ Map.insert identifier value
    mapper input output values = 
      let localEnv = foldr updateEnv env (zip input values) 
      in map (evaluateExpr localEnv) output

evaluatePipeline :: Environment -> Pipeline -> ValueStream -> ValueStream
evaluatePipeline env _ = undefined

applyMultiple
  :: (Monad m, MonadAsync m)
  => Int
  -> ([a] -> [m a])
  -> SerialT m a
  -> SerialT m a
applyMultiple n f s = do
  args <- lift $ S.toList $ adapt $ S.take n s
  let rest = S.drop n s
  if length args < n
    then S.nil
    else (S.fromFoldableM $ f args) <> applyMultiple n f rest
