{-# LANGUAGE OverloadedLists #-}
module Parse.Parser where

import Types
import Text.ParserCombinators.Parsec
import Streamly
import qualified Streamly.Prelude as S
import Data.Ratio
import Control.Monad

pBool :: CharParser st ParsedValue
pBool = (ParsedBool True <$ string "true")
   <|> (ParsedBool False <$ string "false")
   <?> "Boolean value (true or false)"

pString :: CharParser st ParsedValue
pString = try (wholeString '\"')
      <|> (try $ wholeString '\'')
      <|> ParsedString <$> (char ':' *> pIdentifier)
      <?> "String value"
  where 
    wholeString :: Char -> CharParser st ParsedValue
    wholeString delimiter = do
        char delimiter
        content <- stringContent delimiter
        char delimiter
        return $ ParsedString content
    stringContent delimiter = many (quotedChar delimiter)
    quotedChar delimiter = try (delimiter <$ string ['\\', delimiter])
                       <|> noneOf [delimiter]

pNumber :: CharParser st ParsedValue
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
            in ParsedNumber (read (actualSign ++ actualWhole ++ actualFrac) :: Double)

pNone :: CharParser st ParsedValue
pNone = ParsedNone <$ string "none" <?> "None value"

pCollection :: Char -> Char -> CharParser st a -> CharParser st [a]
pCollection start end content = char start *> sepBy (spaces *> content <* spaces) (char ',') <* char end

pList :: CharParser st ParsedValue
pList = ParsedList <$> pCollection '[' ']' pExpression

pStream :: CharParser st ParsedValue
pStream = char '#' *> (ParsedStream <$> pCollection '[' ']' pExpression)

pObject :: CharParser st ParsedValue
pObject = ParsedObject <$> pCollection '{' '}' pObjectPair
  where
    pObjectPair = (,) <$> (pIdentifier <* char ':' <* spaces) <*> pExpression

pProductiveValue :: CharParser st ParsedValue
pProductiveValue = try pAnonymous
     <|> try pBool
     <|> try pNumber
     <|> try pString
     <|> try pNone
     <|> try pList
     <|> try pStream
     <|> pObject
     <?> "Value"

pAnonymous :: CharParser st ParsedValue
pAnonymous = do
    inputs <- sepEndBy1 pIdentifier spaces
    spaces
    char '~'
    spaces
    (ParsedPipe . Anonymous inputs) <$> pExpression

pIdentifier :: CharParser st Identifier
pIdentifier = try ((:) <$> oneOf (lc ++ uc ++ special) <*> many (oneOf (lc ++ uc ++ nums ++ special)))
          <|> choice (map (try . string) operator)
          <?> "Identifier"
    where lc = ['a'..'z']
          uc = ['A'..'Z']
          special = ['_']
          nums = ['0'..'9']
          operator :: [String]
          operator = ["/=","=","+","-","*","/","?","!"]

pExpression :: CharParser st ParsedExpression
pExpression = expr
  where
    productive = (try $ ParsedValue <$> pProductiveValue)
             <|> (try $ ParsedVar <$> pIdentifier)
             <|> inParens pExpression
    expr = try (pTransform productive)
       <|> try (pApplication productive)
       <|> productive

pApplication :: CharParser st ParsedExpression -> CharParser st ParsedExpression
pApplication productive = do
                applicant <- productive
                spaces
                args <- sepEndBy productive spaces
                pure $ ParsedApplication applicant args

pTransform :: CharParser st ParsedExpression -> CharParser st ParsedExpression
pTransform productive = chainl1 pFiller (try (pSpecialTrans "|" "apply") <|> try (pSpecialTrans "<|" "map") <|> pTransformer)
  where
    pSpecialTrans :: String -> String -> CharParser st (ParsedExpression -> ParsedExpression -> ParsedExpression)
    pSpecialTrans sep varName = do
        spaces
        string sep
        spaces
        let transformer = ParsedVar varName
        pure $ (\before after -> ParsedValue $ ParsedPipe $ Transform before transformer after)
    pTransformer = do
        spaces
        char '<'
        spaces
        transformer <- pExpression
        spaces
        char '>'
        spaces
        pure $ (\before after -> ParsedValue $ ParsedPipe $ Transform before transformer after)
    pFiller = (try $ pApplication productive) <|> productive


inParens :: CharParser st a -> CharParser st a
inParens parser = (char '(' *> spaces) *> parser <* (spaces *> char ')')
