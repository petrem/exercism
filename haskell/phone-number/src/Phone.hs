module Phone
  ( number
  ) where

import Control.Applicative
import Text.Trifecta

number :: String -> Maybe String
number s =
  case parseString parsePhoneNumber mempty s of
    (Success parsedNumber) -> Just parsedNumber
    _ -> Nothing

parsePhoneNumber :: (CharParsing f, Monad f) => f String
parsePhoneNumber = try (skipCountry >> skipMany space >> nanp) <|> nanp

nanp :: (CharParsing f, Monad f) => f String
nanp = do
  area <- paranthesized areaOrExchange
  skipMany $ oneOf " -."
  exc <- areaOrExchange
  skipMany $ oneOf " -."
  line <- count 4 digit
  skipMany space
  eof
  return $ concat [area, exc, line]

skipCountry :: (CharParsing f, Monad f) => f ()
skipCountry = skipOptional (char '+') >> skipMany space >> char '1' >> return ()

areaOrExchange :: CharParsing f => f String
areaOrExchange = (:) <$> oneOf ['2' .. '9'] <*> count 2 digit

paranthesized :: CharParsing f => f a -> f a
paranthesized p = skipOptional (char '(') *> p <* skipOptional (char ')')
