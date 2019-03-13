{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Presets where

import Types
import Control.Lens    

class IsPipe a where
  asPipe :: a -> PValue
  transformValue :: a -> [PValue] -> IO PValue
  amountArgs :: a -> Int

instance IsPipe (IO PValue) where
  asPipe f = PPipe $ Builtin [] argsLeft (\staticArgs args -> transformValue f (staticArgs ++ args))
    where
      argsLeft bound = if length bound > 0
        then error "Pipe got too many args"
        else 0
  transformValue f [] = f
  transformValue f _ = error "Pipe got too many args"
  amountArgs = const 0

instance (IsPipe a) => IsPipe (PValue -> a) where
  asPipe f = PPipe $ Builtin [] argsLeft (\staticArgs args -> transformValue f (staticArgs ++ args))
    where
      argsLeft bound = if amountArgs f - length bound < 0
        then error "Pipe got too many args"
        else amountArgs f - length bound
  transformValue f (x:xs) = transformValue (f x) (xs :: [PValue])
  transformValue f [] = error "Pipe didn't get enough arguments"
  amountArgs f = amountArgs (f undefined) + 1

