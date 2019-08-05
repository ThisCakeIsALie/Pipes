{-# LANGUAGE OverloadedLists #-}
module Testing where

import Types
import Text.ParserCombinators.Parsec
import Parse.Parser
import Interpret.Evaluate

testEnv :: Environment
testEnv = [
    ("add",Def (Pipe $ P (const 2) (\_ [Number a,Number b] -> pure $ Number $ a + b)) True),
    ("test",Def (Pipe $ P (const 2) (\_ [a,b] -> print b >> pure a)) True)
    ]

testParse :: String -> ParsedExpression
testParse str = case parse pExpression "" str of
    Left ex -> error (show ex)
    Right result -> result

testEval = evaluateParsedExpr testEnv . testParse