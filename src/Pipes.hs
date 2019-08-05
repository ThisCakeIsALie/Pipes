{-# LANGUAGE OverloadedLists #-}
module Pipes where

import Types
import Parse.Parser
import Interpret.Evaluate
import Text.ParserCombinators.Parsec
import qualified Streamly.Prelude as S
import Data.Map (Map)
import System.IO
import Control.Monad

import Language.Arithmetic
import Language.Control

eval :: String -> IO (Either String Value)
eval input = 
    case parse pExpression "" input of
        Right parsed -> Right <$> evaluateParsedExpr prelude parsed
        Left failed -> pure $ Left $ show failed

prelude :: Environment
prelude = arithmetic <> control

repl :: IO ()
repl = forever $ do
    putStr "> "
    hFlush stdout
    input <- getLine
    result <- eval input
    case result of
        Right value -> print value
        Left error -> putStrLn $ "Encountered error: " ++ error