module Interpret.Environment where

import Types
import Control.Lens
import qualified Data.Map as M

mergeEnv :: Environment -> Environment -> Environment
mergeEnv = M.union

slurpGlobals :: Environment -> Environment -> Environment
slurpGlobals env1 env2 = 
  let
    onlyGlobals = M.filter (^. defGlobal) env2
  in mergeEnv onlyGlobals env1

findDef :: Identifier -> Environment -> Maybe Definition
findDef = M.lookup

define :: Identifier -> Definition -> Environment -> Environment
define = M.insert

defineValue :: Identifier -> Value -> Bool -> Environment -> Environment
defineValue key value global env = define key (Def value global) env

defineAll :: [(Identifier,Definition)] -> Environment -> Environment
defineAll newDefs env = foldr (\(name,value) env -> define name value env) env newDefs

defineAllValues :: [(Identifier,Value)] -> Bool -> Environment -> Environment
defineAllValues newVals global env = defineAll (map (\(key,val) -> (key, Def val global)) newVals) env