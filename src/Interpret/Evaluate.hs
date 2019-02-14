{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
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

  Yield _ _ -> error "Can't yield inside a pipe"

  Apply expr -> do
    evalExpr <- lift $ evaluateExpr env expr
    case evalExpr of
      PPipe pipe -> evaluatePipeInner env pipe s
      value -> yieldValue value

  Anonymous comp argEnv inputs output -> 
    let
      transformer :: [(Identifier, PValue)] -> [PValue] -> IO [PValue]
      transformer evalArgs rest = do
          let localEnv = localizeEnvWithValue (evalArgs ++ zip inputs rest) env
          outputValue <- evaluateExpr localEnv output
          return [fixScope (argEnv ^. defs) outputValue]
    in evalRawPipe argEnv (length inputs) transformer s

  Builtin argEnv argsLeft transformer -> evalRawPipe argEnv (argsLeft argEnv) transformer s

  Connect first rest -> evaluatePipeInner env rest . evaluatePipeInner env first $ s

  Gather first rest -> 
    let
      firstResult = evaluatePipeInner env first s
      packedResult = S.once . return . PList $ firstResult
    in evaluatePipeInner env rest packedResult

  Spread first rest ->
    let 
      firstResult = evaluatePipeInner env first s
    in firstResult >>= spread & evaluatePipeInner env rest

 where

  evalRawPipe argEnv underflow transformer s = do
    evalArgs <- lift $ evalArgsFromEnv argEnv
    if underflow >= 0
      then applyMultiple underflow (transformer evalArgs) s
      else error "Pipe got too many arguments"

  yieldValue value = evalRawPipe (Environment []) 0 (\_ _ -> return $ ([value] :: [PValue])) s

  evalAnonPipe argEnv inputs output s = do
    evalArgs <- lift $ evalArgsFromEnv argEnv
    let underflow = length inputs
        mapper :: [PValue] -> IO [PValue]
        mapper rest = do
          let localEnv = localizeEnvWithValue (evalArgs ++ zip inputs rest) env
          outputValue <- evaluateExpr localEnv output
          --TODO: Hier schauen ob noch args und ist pipe
          return [fixScope (argEnv ^. defs) outputValue]
    if underflow >= 0
      then applyMultiple underflow mapper s
      else error "Pipe got too many arguments"

  evalArgsFromEnv (Environment args) = 
    let
      evalArg (identifier, (Def expr)) = (identifier,) <$> evaluateExpr env expr
    in traverse evalArg (Map.toList args)

  spread (PList list) = list
  spread value        = S.once . return $ value
 
fixScope :: Map Identifier Definition -> PValue -> PValue
fixScope fixes (PPipe pipe) = PPipe $ fixScopePipe fixes pipe
fixScope fixes (PList list) = PList $ S.mapM (return . fixScope fixes) list
fixScope fixes value = value

fixScopePipe :: Map Identifier Definition -> Pipe -> Pipe
fixScopePipe fixes pipe = case pipe of
  Yield exprs rest -> Yield (map (fixScopeExpr fixes) exprs) (fixScopePipe fixes rest)
  Apply expr -> Apply (fixScopeExpr fixes expr)
  Anonymous comp pipeEnv inputs output -> Anonymous comp (pipeEnv & defs %~ (fixes <>)) inputs output
  Builtin boundArgs argsLeft transformer -> Builtin (boundArgs & defs %~ (fixes <>)) ((+ (length fixes)) . argsLeft) transformer
  Connect first second -> Connect (fixScopePipe fixes first) (fixScopePipe fixes second)
  Spread first second -> Spread (fixScopePipe fixes first) (fixScopePipe fixes second)
  Gather first second -> Gather (fixScopePipe fixes first) (fixScopePipe fixes second)

fixScopeExpr :: Map Identifier Definition -> Expression -> Expression
fixScopeExpr fixes (Value value) = Value $ fixScope fixes value
fixScopeExpr fixes (Var var) = case fixes Map.!? var of
  Just (Def value) -> value
  Nothing -> Var var
fixScopeExpr fixes (Application app appArgs) = Application (fixScopeExpr fixes app) (map (fixScopeExpr fixes) appArgs)


evaluatePipeApplication :: Environment -> Pipe -> [Expression] -> IO PValue
evaluatePipeApplication env pipe args = 
  let (updatedPipe, shouldEval) = updatePipeWithArgs env pipe args
  in if shouldEval
    then
      case updatedPipe of
        Yield exprs pipe -> do
          result <- S.head $ evaluatePipeInner env pipe (S.fromFoldable exprs & S.mapM (evaluateExpr env))
          case result of
            Just value -> return value
            Nothing -> error "Pipe did not return a value"

        pipe -> do
          result <- S.head $ evaluatePipeInner env pipe S.nil
          case result of
            Just value -> return value
            Nothing -> error "Pipe did not return a value"

    else return $ PPipe updatedPipe

updatePipeWithArgs :: Environment -> Pipe -> [Expression] -> (Pipe,Bool)
updatePipeWithArgs env pipe args = case pipe of

  yield@(Yield exprs pipe) -> if length args > 0
    then error "Pipe got too many args"
    else (yield, True)

  Apply expr -> 
    let go expr args = case expr of
          Value (PPipe innerPipe) -> updatePipeWithArgs env innerPipe args
          Value value -> if length args == 0
            then (Apply (Value value), True)
            else error "Pipe got too many args"
          Var identifier -> case fetchDef env identifier of
            Just (Def expr) -> go expr args
            Nothing -> error ("The definition '" ++ identifier ++ "' is not in scope")
          Application app appArgs -> go app (appArgs ++ args)
    in go expr args

  Anonymous comp pipeEnv inputs output
    | length inputs < length args -> error "Pipe got too many args"
    | otherwise -> 
      let
        updatedInputs = drop (length args) inputs
        updatedEnv = pipeEnv & defs %~ (Map.fromList (zip inputs (map Def args)) <>)
        updatedPipe = Anonymous comp updatedEnv updatedInputs output
      in (updatedPipe, length inputs == length args)

  Builtin boundArgs argsLeft transformer -> 
    let
      updatedEnv = boundArgs & defs %~ (Map.fromList (zip (map (\n -> "$" ++ show n) [0..]) (map Def args)) <>)
      updatedPipe = Builtin updatedEnv argsLeft transformer
    in (updatedPipe, argsLeft updatedEnv == 0)

  Connect first second -> updateFirstPart Connect first second

  Gather first second -> updateFirstPart Gather first second

  Spread first second -> updateFirstPart Spread first second

  where
    updateFirstPart connector first second = 
      let
        (updatedPipe,shouldEval) = updatePipeWithArgs env first args
      in (connector updatedPipe second, shouldEval)


evaluateExpr :: Environment -> Expression -> IO PValue
evaluateExpr env (Value pval) = return pval
evaluateExpr env (Var identifier) = case fetchDef env identifier of
  Just (Def value) -> evaluateExpr env value
  _ -> error ("The definition '" ++ identifier ++ "' is not in scope")
evaluateExpr env (Application app args) = do
  appValue <- evaluateExpr env app
  case appValue of
    PPipe pipe -> evaluatePipeApplication env pipe args
    _ -> error "Tried to apply a non-pipe value"


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
 

