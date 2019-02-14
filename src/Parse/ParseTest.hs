{-# LANGUAGE OverloadedLists #-}
module Parse.ParseTest where

import Types
import Text.ParserCombinators.Parsec
import Streamly
import qualified Streamly.Prelude as S

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
  where digit = oneOf ['0'..'9']
        asNumber sign whole frac = 
            let actualSign = maybe "" (\a -> [a]) sign
                actualWhole = if whole == "" then "0" else whole
                actualFrac = maybe ".0" (\value -> if value == "" then ".0" else '.':value) frac
            in PNumber (read (actualSign ++ actualWhole ++ actualFrac) :: Double)

pList :: CharParser st PValue
pList = (PList . S.fromFoldable) 
    <$> (char '[' *> sepBy (spaces *> pValue <* spaces) (char ',') <* char ']')

pNone :: CharParser st PValue
pNone = None <$ string "none"

{-
pPipe :: CharParser st Pipe
pPipe = try anonymous
    <|> try apply
    <|> try yield
    <|> connect
  where
    anonymous = do
        inputs <- many (pIdentifier <* spaces)
        spaces *> string "->" *> spaces
        Anonymous [] inputs <$> pExpression
    apply = Apply <$> (char '~' *> pExpression)
    yield = Yield <$> (char '<' *> spaces *> (many $ pExpression <* spaces)) <*> (char '>' *> spaces *> pPipe)
    connect = chainl1 pPipe (spaces *> char '|' *> spaces *> pure Connect)
-}

pPipeRest :: CharParser st Pipe
pPipeRest = try (connected Connect '|')
        <|> try (connected Gather '}')
        <|> try (connected Spread '{')
        <|> pPipePart <* spaces
  where
    connected con sep = con <$> (pPipePart <* spaces <* char sep <* spaces) <*> pPipeRest

pPipe :: CharParser st Pipe
pPipe = do
    yielded <- optionMaybe yield
    case yielded of
        Just yieldedPipe -> yieldedPipe <$> pPipeRest
        Nothing -> pPipeRest
  where
    yield = Yield <$> (char '<' *> spaces *> (many $ pExpression <* spaces) <* spaces <* char '>' <* spaces)
pPipePart :: CharParser st Pipe
pPipePart = try anonymous
        <|> apply
  where
    anonymous = do
        inputs <- many (try (pIdentifier <* (try (many1 space) <|> lookAhead (string "->"))))
        string "->" *> spaces
        Anonymous [] (Environment []) inputs <$> pExpression
    apply = Apply <$> (char ':' *> pExpression)
 

pValue :: CharParser st PValue
pValue = try pBool
     <|> try pNumber
     <|> try pString
     <|> try pNone
     <|> try pList
     <|> PPipe <$> pPipe

pIdentifier :: CharParser st Identifier
pIdentifier = (:) <$> oneOf (lc ++ uc) <*> many (oneOf (lc ++ uc ++ nums ++ ['_']))
          <|> choice (map string operator)
    where lc = ['a'..'z']
          uc = ['A'..'Z']
          nums = ['0'..'9']
          operator :: [String]
          operator = ["+","-","*","/","=","?","!","~"]

pExpression :: CharParser st Expression
pExpression = Value <$> try pValue
          <|> Var <$> try pIdentifier
          <|> inParens (sepBy1 pExpression (many1 space) >>= asApplication)
    where asApplication [] = unexpected "Empty application"
          asApplication (x:xs) = return $ Application x xs

inParens :: CharParser st a -> CharParser st a
inParens parser = (char '(' *> spaces) *> parser <* (spaces *> char ')')

postfixChain :: Parser a -> Parser (a -> a) -> Parser a
postfixChain p op = do
  x <- p
  rest x
  where
    rest x = (do f <- op
                 rest $ f x) <|> return x