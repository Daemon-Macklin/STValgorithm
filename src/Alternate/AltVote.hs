module Alternate.AltVote where

import Data.List 

import CleanData.CleanData

-- Function to count occurances of a vote in a list
countOccurs :: Eq a => a -> [a] -> Int
countOccurs x = length . filter (== x)

-- Function to remove duplicates from a list
removeDups :: Eq a => [a] -> [a]
removeDups []     = []
removeDups (x:xs) = x : filter (/= x) (removeDups xs)

-- Function to extracting unique vote and the number of occurances 
buildUnique :: Ord a => [a] -> [(Int, a)]
buildUnique vs = sort [(countOccurs v vs, v) | v <- removeDups vs]

-- Function to Remove empty lists
removeEmpty :: Eq a => [[a]] -> [[a]]
removeEmpty = filter (/= [])

-- Function to remove any occurances of an vote
removeOccurs :: Eq a => a -> [[a]] -> [[a]]
removeOccurs x = map (filter (/= x))

-- Function to sort the unique votes and the number of occurances
sortUnique :: Ord a => [[a]] -> [a]
sortUnique = map snd . buildUnique . map head

-- Function to find the unique vote with the most number of occurances 
alternativeVote :: Ord a => [[a]] -> a
alternativeVote bs = case sortUnique (removeEmpty bs) of
    [c]    -> c
    (c:cs) -> alternativeVote (removeOccurs c bs)

-- Function to remove wights from votes list and start 
startAltVote :: [Vote] -> String
startAltVote votes = alternativeVote (map (fst) votes)