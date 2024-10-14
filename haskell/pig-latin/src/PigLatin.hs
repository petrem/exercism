{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module PigLatin (translate) where

import Data.Text (Text)
import qualified Data.Text as T

translate :: Text -> Text
translate = T.unwords . map ((`T.append` "ay") . piggify) . T.words

piggify :: Text -> Text
piggify word@(c :> _) | c `elem` ['a', 'e', 'i', 'o', 'u'] = word
piggify word@('x' :> ('r' :> _)) = word
piggify word@('y' :> ('t' :> _)) = word
piggify ('y' :> rest) = T.append (piggify rest) "y"
piggify word =
  let (prefix, suffix) = T.break (`T.elem` "aeiouy") word
   in shuffle prefix suffix
  where
    shuffle (pref :< 'q') ('u' :> suff) = T.concat [suff, pref, "qu"]
    shuffle pref suff = T.append suff pref

pattern (:>) :: Char -> Text -> Text
pattern x :> xs <- (T.uncons -> Just (x, xs))

-- pattern Empty <- (T.uncons -> Nothing)

pattern (:<) :: Text -> Char -> Text
pattern xs :< x <- (T.unsnoc -> Just (xs, x))

-- pattern Empty <- (T.unsnoc -> Nothing)
