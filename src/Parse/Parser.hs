{-# LANGUAGE OverloadedLists #-}
module Parse.Parser where

import Types
import Text.ParserCombinators.Parsec
import Streamly
import qualified Streamly.Prelude as S
import Language.List
import Language.Presets
import Data.Ratio

pBool :: CharParser st PValue
pBool = (PBool True <$ string "true")
   <|> (PBool False <$ string "false")
   <?> "Boolean value (true or false)"

pString :: CharParser st PValue
pString = try (wholeString '\"')
      <|> wholeString '\''
      <?> "String value"
  where 
    wholeString :: Char -> CharParser st PValue
    wholeString delimiter = do
        char delimiter
        content <- stringContent delimiter
        char delimiter
        return $ PString content
    stringContent :: Char -> CharParser st String
    stringContent delimiter = many (quotedChar delimiter)
    quotedChar :: Char -> CharParser st Char
    quotedChar delimiter = try (delimiter <$ string ['\\', delimiter])
                       <|> noneOf [delimiter]

pNumber :: CharParser st PValue
pNumber = asNumber 
      <$> optionMaybe (char '-') 
      <*> many1 digit 
      <*> optionMaybe (char '.' *> many digit)
      <?> "Number value"
  where digit = oneOf ['0'..'9']
        asNumber sign whole frac = 
            let actualSign = maybe "" (\a -> [a]) sign
                actualWhole = if whole == "" then "0" else whole
                actualFrac = maybe ".0" (\value -> if value == "" then ".0" else '.':value) frac
            in PNumber (read (actualSign ++ actualWhole ++ actualFrac) :: Double)

pNone :: CharParser st PValue
pNone = None <$ string "none" <?> "None value"

pPipeRest :: CharParser st Pipe
pPipeRest = try (connected Connect "|")
        <|> try (connected Forward "|>>")
        <|> try (connected Spread "<|")
        <|> try (connected Gather "|>")
        <|> pPipePart <* spaces
        <?> "Anonymous pipe or a connector (e.g. | or |>)"
  where
    connected con sep = con <$> (pPipePart <* spaces <* string sep <* spaces) <*> pPipeRest

pPipe :: CharParser st Pipe
pPipe = char ':' *> spaces *> pPipeRest <?> "Pipe value"

pPipePart :: CharParser st Pipe
pPipePart = try anonymous
        <|> apply
  where
    anonymous = do
        inputs <- many (try (pIdentifier <* (try (many1 space) <|> lookAhead (string "->"))))
        string "->" *> spaces
        Anonymous [] [] inputs <$> pExpression
    apply = Apply <$> pExpression

-- PList is handled as syntactic sugar for the list function
pValue :: CharParser st PValue
pValue = try pBool
     <|> try pNumber
     <|> try pString
     <|> try pNone
     <|> PPipe <$> pPipe
     <?> "Value"

pIdentifier :: CharParser st Identifier
pIdentifier = try ((:) <$> oneOf (lc ++ uc ++ special) <*> many (oneOf (lc ++ uc ++ nums ++ special)))
          <|> choice (map string operator)
          <?> "Identifier"
    where lc = ['a'..'z']
          uc = ['A'..'Z']
          special = ['_']
          nums = ['0'..'9']
          operator :: [String]
          operator = ["/=","+","-","*","/","=","?","!","~"]

pExpression = try (sepEndBy1 innerExpr (try (many1 space)) >>= asApplication)
           <|> try (Value <$> pValue)
           <|> try (Var <$> pIdentifier)
           <|> syntacticSugar
           <?> "Expression"
  where asApplication [] = unexpected "Empty application"
        asApplication (x:xs) = return $ Application x xs
        innerExpr = try (inParens (try pExpression <|> innerExpr))
            <|> try (Value <$> pValue)
            <|> try (Var <$> pIdentifier)
            <|> syntacticSugar


syntacticSugar :: CharParser st Expression
syntacticSugar = (\x -> Application (Value list) x) 
    <$> (char '[' *> sepBy (spaces *> pExpression <* spaces) (char ',') <* char ']')


inParens :: CharParser st a -> CharParser st a
inParens parser = (char '(' *> spaces) *> parser <* (spaces *> char ')')
