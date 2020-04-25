module Alternate.AltVote where

import Data.List 

import CleanData.CleanData

-- Count occurances of an element in a list
countOccurances :: Eq a => a -> [a] -> Int
countOccurances x = length . filter (== x)

-- Remove duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : filter (/= x) (removeDuplicates xs)

-- Extracting the unique elements and the number of occurances in it from a list as a two tuple
buildUniqueVotes :: Ord a => [a] -> [(Int, a)]
buildUniqueVotes vs = sort [(countOccurances v vs, v) | v <- removeDuplicates vs]

-- Remove any empty lists from a master list
removeEmptyLists :: Eq a => [[a]] -> [[a]]
removeEmptyLists = filter (/= [])

-- Remove any occurances of an element in list inside lists
removeOccurances :: Eq a => a -> [[a]] -> [[a]]
removeOccurances x = map (filter (/= x))

-- Sort the unique elements and the number of occurances in it from a list
sortUniqueVotes :: Ord a => [[a]] -> [a]
sortUniqueVotes = map snd . buildUniqueVotes . map head

-- Find the unique element with the most number of occurances in it from a list
alternativeVote :: Ord a => [[a]] -> a
alternativeVote bs = case sortUniqueVotes (removeEmptyLists bs) of
    [c]    -> c
    (c:cs) -> alternativeVote (removeOccurances c bs)

startAltVote :: [Vote] -> String
startAltVote votes = alternativeVote (map (fst) votes)