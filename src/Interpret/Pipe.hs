{-# LANGUAGE FlexibleContexts #-}
module Interpret.Pipe where

import Streamly
import qualified Streamly.Prelude as S
import Types
import Interpret.Environment
import Control.Monad.Trans (lift)
import Control.Lens
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Interpret.Expression

--TODO: take elements from stream if arguments underflow
--TODO: If a non element is found
--TODO: Replace Call with Expr and specialize current to Var
evaluatePipe :: Environment -> PipeCall -> ValueStream -> ValueStream
evaluatePipe env (Call identifier args) s = case fetchAny identifier of

  Just (Right (PPipeline line)) -> 
    if length args == 0
      then evaluatePipeline env line s
      else error "This pipeline is fully evaluated"
  
  Just (Right value) -> undefined

  Just (Left  (Def argDefs expr)) -> do
    evalArgs <- lift $ traverse (evaluateExpr env) args
    let underflow = length argDefs - length args
        mapper values = do
          undefined
    undefined
    {-
    rest <- if underflow > 0
              then lift $ S.toList $ S.take underflow s
              else return []
    if length rest == underflow
      then do
        let localEnv = localizeEnv (zip argDefs (evalArgs ++ rest)) env
        evalResult <- lift $ evaluateExpr localEnv expr
        case evalResult of
          PPipeline line -> evaluatePipeline env line (S.drop underflow s)
          value -> S.cons value (S.drop underflow s)
      else S.nil
    -}
  Just (Left (Builtin transformer)) -> undefined

  Just _  -> S.once . return $ PError (identifier ++ " is not a pipe")

  Nothing -> S.once . return $ PError (identifier ++ " is not defined")

  where
    fetchAny identifier = case fetchVar env identifier of
      Nothing -> Left <$> fetchDef env identifier
      var -> Right <$> var 



evaluatePipe env (Local (Pipe comp inputs outputs) staticArgs) s = do
  evalStatic <- lift $ traverse (evaluateExpr env) staticArgs
  let dynamicSize = length inputs - length evalStatic 
      staticSize = length evalStatic
  if dynamicSize >= 0
    then let staticEnv = zip (take staticSize inputs) evalStatic
         in  applyMultiple dynamicSize (mapper staticEnv (drop staticSize inputs) outputs) s
    else S.once . return $ PError "Anonymous call got too many arguments"

 where
  mapper static input output values =
    let localEnv = localizeEnv (static ++ zip input values) env
    in  traverse (evaluateExpr localEnv) output

{-

evaluatePipe env (Call identifier args) s = case fetchAny identifier of

  Just (Right (PPipeline line  )) -> evaluatePipeline env line s

  Just (Left  (Def argDefs expr)) -> if length argDefs == length args

    then do

      let localEnv = localizeEnv (zip argDefs args) env

      evaluated <- lift $ evaluateExpr localEnv expr

      case evaluated of

        PPipeline line -> evaluatePipeline env line s

        _ -> S.once . return $ PError (identifier ++ " is not a pipe")

    else S.once . return $ PError

      ("Invalid amount of arguments given to " ++ identifier)

  Just _  -> S.once . return $ PError (identifier ++ " is not a pipe")

  Nothing -> S.once . return $ PError (identifier ++ " is not defined")

 where

  fetchAny identifier = case fetchVar env identifier of

    Nothing -> Left <$> fetchDef env identifier

    var     -> Right <$> var



evaluatePipe env (Anonymous comp) s =

  let inputs    = mapMaybe (^? _In) comp

      inputSize = length inputs

      outputs   = mapMaybe (^? _Out) comp

  in  applyMultiple inputSize (mapper inputs outputs) s

 where

  mapper input output values =

    let localEnv = localizeEnv (zip input values) env

    in  map (evaluateExpr localEnv) output

-}

updateEnv :: (Identifier, PValue) -> Environment -> Environment
updateEnv (identifier, value) env = env & vars %~ Map.insert identifier value

localizeEnv :: [(Identifier, PValue)] -> Environment -> Environment
localizeEnv toUpdate env = foldr updateEnv env toUpdate

evaluatePipeline :: Environment -> Pipeline -> ValueStream -> ValueStream
evaluatePipeline env End                 s = s
evaluatePipeline env (Connect pipe rest) s = apply env pipe rest s
evaluatePipeline env (Gather pipe rest) s =
  PList s & return & S.once & evaluatePipe env pipe
evaluatePipeline env (Spread pipe rest) s =
  s >>= spread & evaluatePipe env pipe
 where
  spread (PList list) = list
  spread value        = S.once . return $ value

apply :: Environment -> PipeCall -> Pipeline -> ValueStream -> ValueStream
apply env pipe rest = evaluatePipeline env rest . evaluatePipe env pipe

applyMultiple
  :: (Monad m, MonadAsync m)
  => Int
  -> ([a] -> m [a])
  -> SerialT m a
  -> SerialT m a

applyMultiple 0 f s = do
  let result = f []
  empty <- lift $ S.null s
  if empty
    then asStream result
    else do
      val <- s
      asStream $ (<> [val]) <$> f []

applyMultiple n f s = do
  args <- lift $ S.toList $ adapt $ S.take n s
  let rest = S.drop n s
  if length args < n
    then S.nil
    else (asStream $ f args) <> applyMultiple n f rest

asStream :: (Monad m, MonadAsync m) => m [a] -> SerialT m a
asStream xs = do
  list <- lift xs
  S.fromFoldable list