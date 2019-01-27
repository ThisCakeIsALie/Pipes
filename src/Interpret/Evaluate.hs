{-# LANGUAGE FlexibleContexts #-}
module Interpret.Evaluate where
 
import Streamly
import qualified Streamly.Prelude as S
import Types
import Interpret.Environment
import Control.Monad.Trans (lift)
import Control.Lens
import Control.Applicative ((<|>))
import qualified Data.Map as Map
 
--TODO: If a non element is found
--TODO: Replace Call with Expr and specialize current to Var
evaluatePipe :: Environment -> [Expression] -> Pipe -> ValueStream -> ValueStream
evaluatePipe env args pipeCall s = case pipeCall of

  Call identifier -> case fetchDef env identifier of
    Just (Def pipe) -> evaluatePipe env args pipe s
    Just (Builtin transformer) -> transformer env args s
    Nothing -> error $ identifier ++ " is not defined"

  Pipe comp inputs outputs -> evalRawPipe inputs outputs args s 

  Connect first rest -> evaluatePipe env [] rest . evaluatePipe env args first $ s

  Gather first rest -> 
    let
      firstResult = evaluatePipe env args first s
      packedResult = S.once . return . PList $ firstResult
    in evaluatePipe env [] rest packedResult

  Spread first rest ->
    let 
      firstResult = evaluatePipe env args first s
    in firstResult >>= spread & evaluatePipe env [] rest

 where

  mapper static input output values =
    let localEnv = localizeEnv (zip input values ++ static) env
    in  traverse (evaluateExpr localEnv) output

  evalRawPipe inputs outputs args s = do
    evalArgs <- lift $ traverse (evaluateExpr env) args
    let underflow = length inputs - length args
        mapper rest = do
          let localEnv = localizeEnv (zip inputs (evalArgs ++ rest)) env
          traverse (evaluateExpr localEnv) outputs
    if underflow >= 0
      then applyMultiple underflow mapper s
      else error "Pipe got too many arguments"

  spread (PList list) = list
  spread value        = S.once . return $ value
 
asDefinition :: PValue -> Definition
asDefinition value = Def (Pipe [] [] [Value value])
 
updateEnv :: (Identifier, PValue) -> Environment -> Environment
updateEnv (identifier, value) env = env & defs %~ Map.insert identifier (asDefinition value)
 
localizeEnv :: [(Identifier, PValue)] -> Environment -> Environment
localizeEnv toUpdate env = foldr updateEnv env toUpdate
 
evaluateExpr :: Environment -> Expression -> IO PValue
evaluateExpr env (Value pval      ) = return pval
evaluateExpr env (Var identifier) = undefined
  {-case fetchDef env identifier of
  Just (Def (Pipe comp inputs outputs)) -> if length args == length inputs
    then do
      evalArgs <- traverse (evaluateExpr env) args
      let localEnv = localizeEnv (zip inputs evalArgs) env
      if length outputs <= 1
        then evaluateExpr localEnv (head outputs)
        else return $ PList $ asStream $ traverse (evaluateExpr localEnv) outputs
    else error "Wrong number of parameters given to definition"
  Nothing -> error "Definition not found"
  -}
evaluateExpr env (Application (PPipe pipe) args) = return $ PList $ evaluatePipe env args pipe S.nil
evaluateExpr env (Application _ _) = error "Tried to apply a value which is not a Pipeline"
 
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
    else error "Can't use a generator pipe on a nonempty list"
 
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
 

