{-# LANGUAGE FlexibleInstances #-}
module Language.Native where

import Types
import Data.Map (Map)
import qualified Data.Map as M
import Streamly
import qualified Streamly.Prelude as S

class Native a where
  asValue :: a -> Value

class NativePipe a where
  asPipe :: a -> Value
  pipeArgs :: a -> Int

instance Native Value where
    asValue = id

instance Native String where
    asValue = String

instance Native Double where
    asValue = Number

instance Native Bool where
    asValue = Bool

instance (Native a) => Native [a] where
    asValue = List . map asValue

instance (Native a) => Native (Map Identifier a) where
    asValue = Object . M.map asValue

instance (Native a) => Native [(Identifier, a)] where
    asValue = Object . M.map asValue . M.fromList

instance (Native a, IsStream s) => Native (s Runtime a) where
    asValue = Stream . S.mapM (pure . asValue) . adapt

instance (Native a) => NativePipe (Runtime a) where
    pipeArgs = const 0
    asPipe x = Pipe $ P (const 0) (transformer x)
      where
        transformer x _ _ = asValue <$> x

instance (NativePipe a) => NativePipe (Value -> a) where
    asPipe f = Pipe $ P (const $ pipeArgs f) (transformer f)
      where
        transformer f currEnv (x:xs) = 
            let Pipe (P _ pipeFunc) = asPipe . f $ x
            in pipeFunc currEnv xs
    pipeArgs f = 1 + (pipeArgs $ f undefined)

instance (NativePipe a) => NativePipe (Environment -> a) where
    asPipe f = Pipe $ P (const $ pipeArgs f) (transformer f)
      where
        transformer f currEnv args =
            let Pipe (P _ pipeFunc) = asPipe $ f currEnv
            in pipeFunc currEnv args
    pipeArgs f = pipeArgs $ f undefined


nativeToDef :: (Native a) => Bool -> Identifier -> a -> (Identifier,Definition)
nativeToDef global name x = (name,Def (asValue x) global)

globalValue :: (Native a) => Identifier -> a -> (Identifier,Definition)
globalValue = nativeToDef True

nativePipeToDef :: (NativePipe a) => Bool -> Identifier -> a -> (Identifier,Definition)
nativePipeToDef global name x = (name,Def (asPipe x) global)

globalPipe :: (NativePipe a) => Identifier -> a -> (Identifier,Definition)
globalPipe = nativePipeToDef True