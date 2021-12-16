module WordProblem (
--  answer
  ) where

import Text.Parsec

data Operator = Plus
              | Minus
              deriving (Show)

type Number = Integer

data Expression = Terminal Number
                | Node Expression Operator Expression

-- answer :: String -> Integer
-- answer problem = evaluateExpression . parseProblem


-- Parsers

parseProblem :: Parsec String st Integer
parseProblem = numberParser

numberParser :: Parsec String st Number
numberParser = read <$> many (oneOf "0123456789")

--parseProblem :: Parsec String st [String]
--parseProblem = (:) <$> wordParser <*>  many (char ' ' *> wordParser)

--parseProblem :: Parsec String st String
--parseProblem = (string "dog") <|> (string "cat")

wordParser:: Parsec String st String
wordParser = many $ oneOf $ ['a'..'z'] ++ ['A'..'Z']

-- Expressions
evaluateExpression :: Expression -> Integer
evaluateExpression expr = 0
