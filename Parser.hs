module Parser(expressionParser, programParser) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Control.Monad
import Grammar

ws :: Parser String
ws = many . char $ ' '

oneOf' :: [Parser a] -> Parser a
oneOf' = choice . (map try)

oneOfWords :: [String] -> Parser String
oneOfWords = oneOf' . (map Text.ParserCombinators.Parsec.string)

lambda :: Parser String
lambda = oneOfWords ["Lambda", "lambda", "L", "λ"]

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

englishCharSequence :: Parser [Char]
englishCharSequence = many1 . oneOf' . (map char) $ ['a'..'z']

-- Untyped Lambda Calculus Parsers --

variable :: Parser Expression
variable =
  do
    v <- englishCharSequence
    return $ Var v

application :: Parser Expression
application = parens app'
  where
    app' =
          do
            ws
            e1 <- expressionParser
            ws
            e2 <- expressionParser
            ws
            return $ App e1 e2
 
abstraction :: Parser Expression
abstraction = parens abs'
  where 
    abs' =
          do
            lambda
            ws
            v <- englishCharSequence
            ws
            char '.'
            ws
            e <- expressionParser
            return $ Abs v e

expressionParser :: Parser Expression
expressionParser = oneOf' [variable, abstraction, application]

-- Let expressions augmentation parsers --

definition :: Parser Definition
definition =
  do
    v <- englishCharSequence
    ws
    char '='
    ws
    e <- expressionParser
    char '\n'
    return $ (v, e)

context :: Parser Context
context = Map.fromList <$> many definition

programParser :: Parser Program
programParser =
  do
    ctx <- context
    char '\n'
    exp <- expressionParser
    return $ (ctx, exp)
