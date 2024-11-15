module FoodChain (song) where

import Data.List (intercalate, unfoldr)
import Data.Maybe (catMaybes)


song :: String
song = unlines $ intercalate [""] (stanza <$> animalDefs)

data Animal = Regular   { name :: String, uttering :: String }
            | Qualified { name :: String, uttering :: String }
            | Last      { name :: String, uttering :: String }  deriving (Eq, Show)

animalDefs :: [Animal]
animalDefs = [ Last "fly" "I don't know why she swallowed the fly. Perhaps she'll die."
             , Qualified "spider" "wriggled and jiggled and tickled inside her"
             , Regular "bird" "How absurd to swallow a bird!"
             , Regular "cat" "Imagine that, to swallow a cat!"
             , Regular "dog" "What a hog, to swallow a dog!"
             , Regular "goat" "Just opened her throat and swallowed a goat!"
             , Regular "cow" "I don't know how she swallowed a cow!"
             , Last "horse" "She's dead, of course!" ]

stanza :: Animal -> [String]
stanza animal = (concat ["I know an old lady who swallowed a ", name animal, "."]) : accretion
  where
    accretion = catMaybes $ unfoldr go (Nothing, dropWhile (/= animal) . reverse $ animalDefs)
    
    go (Nothing,         [])                  = Nothing
    go (Nothing,         (a@(Last _ _):rest)) = Just (Nothing, (Just a, rest))
    go (Nothing,         (a:rest))            = Just (Just $ utter a, (Just a, rest))
    go (Just (Last _ u), _)                   = Just (Just $ u, (Nothing, []))
    go (Just prev,       [])                  = error $ "Bug. The last animal, " ++ name prev ++ ", was not a true Last."
    go (Just prev,       (a:rest))            = Just (Just $ explain prev a, (Just a, rest))

    utter (Qualified _ u) = concat ["It ", u, "."]
    utter a = uttering a
        
    explain a1 a2 = concat ["She swallowed the ", name a1, " to catch the ", qualified a2, "."]

    qualified (Qualified n u) = concat [n, " that ", u]
    qualified a = name a
