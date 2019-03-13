{-# LANGUAGE Rank2Types #-}
module Interpret.Environment where

import Types
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens
import qualified Streamly.Prelude as S

fetchDef :: Environment -> Identifier -> Maybe Definition
fetchDef env identifier = Map.lookup identifier env

updateEnvWithValue :: (Identifier, PValue) -> Environment -> Environment
updateEnvWithValue (identifier, value) = Map.insert identifier value

localizeEnvWithValue :: [(Identifier, PValue)] -> Environment -> Environment
localizeEnvWithValue toUpdate env = foldr updateEnvWithValue env toUpdate
 
propagateEnvInValue :: Environment -> PValue -> PValue
propagateEnvInValue fixes (PPipe pipe) = PPipe $ propagateEnvInPipe fixes pipe
propagateEnvInValue fixes (PList list) = PList $ S.mapM (return . propagateEnvInValue fixes) list
propagateEnvInValue fixes value = value

propagateEnvInPipe :: Environment -> Pipe -> Pipe
propagateEnvInPipe fixes pipe = case pipe of
  Apply expr -> Apply (propagateEnvInExpr fixes expr)
  Anonymous env bound inputs output -> Anonymous (fixes <> env) bound inputs output
  Builtin boundArgs argsLeft transformer -> Builtin boundArgs argsLeft transformer
  Connect first second -> connectPropagated Connect first second
  Spread first second -> connectPropagated Spread first second
  Gather first second -> connectPropagated Gather first second
  Forward first second -> connectPropagated Forward first second
  where
    connectPropagated connector first second = 
      let
        propagatedFirst = propagateEnvInPipe fixes first
        propagatedSecond = propagateEnvInPipe fixes second
      in connector propagatedFirst propagatedSecond

propagateEnvInExpr :: Environment -> Expression -> Expression
propagateEnvInExpr fixes (Value value) = Value $ propagateEnvInValue fixes value
propagateEnvInExpr fixes (Var var) = case fixes Map.!? var of
  Just value -> Value value
  Nothing -> Var var
propagateEnvInExpr fixes (Application app appArgs) = Application (propagateEnvInExpr fixes app) (map (propagateEnvInExpr fixes) appArgs)