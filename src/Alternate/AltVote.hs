module Alternate.AltVote where

import Data.List 

import CleanData.CleanData

-- Count occurances of an element in a list
countOccurs :: Eq a => a -> [a] -> Int
countOccurs x = length . filter (== x)

-- Remove duplicates from a list
removeDups :: Eq a => [a] -> [a]
removeDups []     = []
removeDups (x:xs) = x : filter (/= x) (removeDups xs)

-- Extracting the unique elements and the number of occurances in it from a list as a two tuple
buildUnique :: Ord a => [a] -> [(Int, a)]
buildUnique vs = sort [(countOccurs v vs, v) | v <- removeDups vs]

-- Remove any empty lists from a master list
removeEmpty :: Eq a => [[a]] -> [[a]]
removeEmpty = filter (/= [])

-- Remove any occurances of an element in list inside lists
removeOccurs :: Eq a => a -> [[a]] -> [[a]]
removeOccurs x = map (filter (/= x))

-- Sort the unique elements and the number of occurances in it from a list
sortUnique :: Ord a => [[a]] -> [a]
sortUnique = map snd . buildUnique . map head

-- Find the unique element with the most number of occurances in it from a list
alternativeVote :: Ord a => [[a]] -> a
alternativeVote bs = case sortUnique (removeEmpty bs) of
    [c]    -> c
    (c:cs) -> alternativeVote (removeOccurs c bs)

startAltVote :: [Vote] -> String
startAltVote votes = alternativeVote (map (fst) votes)