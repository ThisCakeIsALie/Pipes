{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
module Interpret.Evaluate where
 
import Streamly
import qualified Streamly.Prelude as S
import Types
import Interpret.Environment
import Control.Monad.Trans (lift)
import Control.Lens
import Control.Applicative ((<|>))
import qualified Data.Map as Map
import Data.Map (Map)
 
--TODO: If a none element is found
evaluatePipeInner :: Environment -> Pipe -> ValueStream -> ValueStream
evaluatePipeInner env pipe s = case pipe of

  Apply expr -> do
    evalExpr <- lift $ evaluateExpr env expr
    case evalExpr of
      PPipe pipe -> evaluatePipeInner env pipe s
      value -> yieldValue value

  Anonymous closure boundArgs inputs output -> 
    let
      transformer evalArgs rest = do
          let values = Map.toList closure ++ zip inputs (evalArgs ++ rest)
          let localEnv = localizeEnvWithValue values env
          outputValue <- evaluateExpr localEnv output
          return $ propagateEnvInValue localEnv outputValue
    in do
      evalArgs <- lift $ traverse (evaluateExpr env) boundArgs
      evalRawPipe (length inputs - length evalArgs) (transformer evalArgs) s

  Builtin boundArgs argsLeft transformer -> do
    evalArgs <- lift $ traverse (evaluateExpr env) boundArgs
    evalRawPipe (argsLeft boundArgs) (transformer evalArgs) s

  Connect first rest -> evaluatePipeInner env rest . evaluatePipeInner env first $ s

  Gather first second -> do
    drained <- lift $ S.toList $ evaluatePipeInner env first s
    evaluatePipeInner env second (return $ PList $ S.fromFoldable drained)

  Forward first rest -> 
    let
      firstResult = evaluatePipeInner env first s
      packedResult = return . PList $ firstResult
    in evaluatePipeInner env rest packedResult

  Spread first rest ->
    let 
      firstResult = evaluatePipeInner env first s
    in firstResult >>= spread & evaluatePipeInner env rest

 where
  evalRawPipe underflow transformer s =
    if underflow >= 0
      then applyMultiple underflow transformer s
      else error "Pipe got too many arguments"

  yieldValue value = evalRawPipe 0 (\_ -> return value) s

  spread (PList list) = list
  spread value        = return value
 


evaluatePipeApplication :: Environment -> Pipe -> [Expression] -> IO PValue
evaluatePipeApplication env pipe args = 
  let (updatedPipe, shouldEval) = updatePipeWithArgs env pipe args
  in if shouldEval
    then do
      result <- S.head $ evaluatePipeInner env updatedPipe S.nil
      case result of
        Just value -> return value
        Nothing -> error "Pipe did not return a value"

    else return $ PPipe updatedPipe

updatePipeWithArgs :: Environment -> Pipe -> [Expression] -> (Pipe,Bool)
updatePipeWithArgs env pipe args = case pipe of

  Apply expr -> 
    let go expr args = case expr of
          Value (PPipe innerPipe) -> updatePipeWithArgs env innerPipe args
          Value value -> if length args == 0
            then (Apply (Value value), True)
            else error "Pipe got too many args"
          Var identifier -> case fetchDef env identifier of
            Just value -> go (Value value) args
            Nothing -> error ("The definition '" ++ identifier ++ "' is not in scope")
          Application app appArgs -> go app (appArgs ++ args)
    in go expr args

  Anonymous closure bound inputs output
    | length inputs < length bound + length args -> error "Pipe got too many args"
    | otherwise -> 
      let
        updatedBound = bound ++ args
        updatedPipe = Anonymous closure updatedBound inputs output
      in (updatedPipe, length updatedBound == length inputs)

  Builtin boundArgs argsLeft transformer -> 
    let
      updatedArgs = boundArgs ++ args
      updatedPipe = Builtin updatedArgs argsLeft transformer
    in (updatedPipe, argsLeft updatedArgs == 0)

  Connect first second -> updateFirstPart Connect first second

  Gather first second -> updateFirstPart Gather first second

  Spread first second -> updateFirstPart Spread first second

  Forward first second -> updateFirstPart Forward first second

  where
    updateFirstPart connector first second = 
      let
        (updatedPipe,shouldEval) = updatePipeWithArgs env first args
      in (connector updatedPipe second, shouldEval)


evaluateExpr :: Environment -> Expression -> IO PValue
evaluateExpr env (Value pval) = return pval
evaluateExpr env (Var identifier) = case fetchDef env identifier of
  Just value -> return value
  _ -> error ("The definition '" ++ identifier ++ "' is not in scope")
evaluateExpr env (Application app args) = do
  appValue <- evaluateExpr env app
  case appValue of
    PPipe pipe -> evaluatePipeApplication env pipe args
    value -> 
      if length args == 0
        then return value
        else error "Pipe got too many arguments"


applyMultiple :: (Monad m, MonadAsync m) => Int -> ([a] -> m a) -> SerialT m a -> SerialT m a

applyMultiple 0 f s = do
  let result = f []
  empty <- lift $ S.null s
  if empty
    then S.once result
    else error "Can't use a generator pipe on a nonempty list"

applyMultiple n f s = go [] s
  where 
    go args rest
      | length args == n = f args S.|: go [] rest
      | otherwise = do
          destructured <- lift $ S.uncons rest
          case destructured of
            Just (x,xs) -> go (args ++ [x]) xs
            Nothing -> S.nil
