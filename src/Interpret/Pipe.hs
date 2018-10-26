{-# LANGUAGE FlexibleContexts #-}
module Interpret.Pipe where

import Streamly
import qualified Streamly.Prelude as S
import Types
import Interpret.Environment
import Control.Monad.Trans
import Control.Lens
import Data.Maybe (mapMaybe)

evaluatePipe :: Environment -> Pipe -> Runtime PValue -> Runtime PValue
evaluatePipe env (Call identifier args) s = case fetchVar env identifier of
  Just (PPipeline line) -> evaluatePipeline env line $ s --TODO: fetch from Defs as well and update s for recursive call

  Just _ -> lift . S.once . return $ PError "Tried to apply a non-pipe value"
  _ -> lift . S.once . return $ PError
    ("Variable \"" ++ identifier ++ "\" not bound")
evaluatePipe env (Anonymous comp) s =
  let inputs  = mapMaybe (^? _In) comp
      outputs = mapMaybe (^? _Out) comp
      mapper values = undefined
  in  undefined --TODO: Implement this


evaluatePipeline :: Environment -> Pipeline -> Runtime PValue -> Runtime PValue
evaluatePipeline env _ = undefined

applyMultiple :: (Monad m, MonadAsync m) => Int -> ([a] -> [a]) -> SerialT m a -> SerialT m a
applyMultiple n f s = do
  args <- lift $ S.toList $ adapt $ S.take n s
  let rest = S.drop n s
  if length args < n then S.nil else (S.fromFoldable $ f args) <> applyMultiple n f rest
