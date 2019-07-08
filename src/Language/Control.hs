{-# LANGUAGE OverloadedLists #-}
module Language.Control where

import Types
import Language.Presets
import qualified Streamly.Prelude as S
import Data.Map (Map)

controlBuiltins :: Map Identifier Definition
controlBuiltins = [
    ("~", asPipe idValue),
    ("drain", asPipe drain),
    ("gather", asPipe gather),
    ("spread", asPipe spread)
  ]

idValue :: PValue -> IO PValue
idValue = return

drain :: PValue -> IO PValue
drain (PList list) = do
  drained <- S.toList list
  return $ PList $ S.fromFoldable drained

gather :: PValue -> IO PValue
gather = return . PList . S.once . return

spread :: PValue -> IO PValue
spread (PList list) = return . PList $ list >>= spreadValue
  where
    spreadValue (PList list) = list
    spreadValue value        = return value