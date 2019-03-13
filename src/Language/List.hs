{-# LANGUAGE OverloadedLists #-}
module Language.List where

import Types
import Language.Presets
import Data.Map (Map)
import qualified Streamly.Prelude as S
import Interpret.Evaluate

listBuiltins :: Environment -> Map Identifier Definition
listBuiltins env = [("range", asPipe range), ("take3", asPipe take3), ("list", list),("all", asPipe $ allVal env),("/=", asPipe neVal)]

list :: PValue
list = PPipe $ Builtin [] (const 0) transformer
  where
    transformer static dynamic = return $ PList (S.fromFoldable (static ++ dynamic))

range :: PValue -> PValue -> IO PValue
range (PNumber a) (PNumber b) = return $ PList $ S.fromFoldable (map (PNumber . fromInteger ) [floor a..floor b])

allVal :: Environment -> PValue -> PValue -> IO PValue
allVal env (PPipe pipe) (PList list) = PBool <$> S.foldrM (\x rest -> ((&& rest) . truthify) <$> (evaluatePipeApplication env pipe [Value x])) True list

take3 :: PValue -> IO PValue
take3 (PList list) = return $ PList $ S.take 3 list

truthify :: PValue -> Bool
truthify (PBool val) = val
truthify _ = False

neVal :: PValue -> PValue -> IO PValue
neVal (PNumber a) (PNumber b) = return $ PBool $ a /= b