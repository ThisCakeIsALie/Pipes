{-# LANGUAGE OverloadedLists #-}
module Pipes where

import Types
import Parse.ParseTest
import Interpret.Evaluate
import Text.ParserCombinators.Parsec
import qualified Streamly.Prelude as S
import Data.Map (Map)
import Language.Arithmetic
import Language.IO
import Language.Control

evalExpr :: String -> IO PValue
evalExpr input = 
    case parse pExpression "" input of
        Right parsed -> evaluateExpr prelude parsed
        Left failed -> error (show failed)

prelude :: Environment
prelude = Environment (arithmeticBuiltins <> ioBuiltins <> controlBuiltins)

succValue :: PValue -> PValue
succValue (PNumber n) = PNumber (n+1)
