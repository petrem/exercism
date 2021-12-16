{-# LANGUAGE OverloadedStrings #-}

module House (rhyme) where

import Data.List (intercalate)


data SPO = SPO { subject :: String
               , predicate :: String
               , object :: NP
               } deriving (Show)

data RCl = RCl SPO | End deriving (Show)

data NP = NP { headWord :: String
             , rCl :: RCl
             } deriving (Show)

rhyme :: String
rhyme = intercalate "\n" $ map showSPO verses

verses :: [SPO]
verses = scanl embed initialStory subStories

initialStory :: SPO
initialStory = SPO "This" "is" (NP "house that Jack built" End)

subStories :: [(String, String)]
subStories = [ ("malt", "lay in")
             , ("rat", "ate")
             , ("cat", "killed")
             , ("dog", "worried")
             , ("cow with the crumpled horn", "tossed")
             , ("maiden all forlorn", "milked")
             , ("man all tattered and torn", "kissed")
             , ("priest all shaven and shorn", "married")
             , ("rooster that crowed in the morn", "woke")
             , ("farmer sowing his corn", "kept")
             , ("horse and the hound and the horn", "belonged to")
             ]

makeRCl :: String -> String -> NP -> NP
makeRCl hw verb np = NP hw $ RCl (SPO  "that" verb np)

embed :: SPO -> (String, String) -> SPO
embed (SPO s p o) (hw, verb) = SPO s p (makeRCl hw verb o)

showSPO :: SPO -> String
showSPO (SPO s p o) = unwords [s, p, showNP o]

showNP :: NP -> String
showNP np = "the " ++ headWord np ++ showRCl (rCl np)

showRCl :: RCl -> String
showRCl (RCl spo) = "\n" ++ showSPO spo
showRCl End = ".\n"

