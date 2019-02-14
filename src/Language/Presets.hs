{-# LANGUAGE OverloadedLists #-}
module Language.Presets where

import Types
import Control.Lens    

unaryBuiltin :: (PValue -> IO PValue) -> PValue
unaryBuiltin f = PPipe $ Builtin (Environment []) argsLeft transformer
  where
    argsLeft env = case length (env ^. defs) of
        0 -> 1
        1 -> 0
        _ -> error "Pipe got too many args"
        
    transformer :: [(Identifier,PValue)] -> [PValue] -> IO [PValue]
    transformer staticArgs args =  case staticArgs of
        [(_,val)] -> (\x -> [x]) <$> f val
        [] -> (\x -> [x]) <$> f (head args)