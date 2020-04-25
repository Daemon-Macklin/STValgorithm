module CleanData.CleanData where

import Data.List.Split (splitOn)

type Candidate = (String, [Vote])
type Result = (String, Double)
type Vote = ([String], Double)

weight :: Double
weight = 1000

toInt :: String -> Int
toInt string = read string::Int

getCandidates :: String -> [Candidate]
getCandidates rawVotes = [(x, []) | x <- drop 2 (head $ dirtyVotes rawVotes)]

getQuota :: String -> String -> Int
getQuota rawVotes seats = ((length $ formatVotes rawVotes) `div` ((toInt seats) + 1)) + 1

formatVotes :: String -> [Vote]
formatVotes rawVotes = filter ((/= []).fst) (map (formatVote) (sortVotes rawVotes))

formatVote :: [(String, String)] -> Vote
formatVote vote = ([fst x | x <- vote, snd x /= "*"], weight)

sortVotes :: String -> [[(String, String)]]
sortVotes rawVotes = map (isort) (cleanVotes rawVotes) 

isort :: Ord a => [(String, a)] -> [(String, a)]
isort [] = []
isort (x:xs) = insertionSort x (isort xs)

insertionSort :: Ord a => (String, a) -> [(String, a)]  -> [(String, a)]
insertionSort x [] = [x]
insertionSort x (y:ys) 
                | snd x <= snd y = x : y : ys
                | otherwise = y: insertionSort x ys

zipVote :: [String] -> [String] -> [(String, String)]
zipVote candidates vote = zip (drop 2 candidates) (drop 2 vote)

cleanVotes :: String -> [[(String, String)]]
cleanVotes rawVotes = map (zipVote (head $ dirtyVotes rawVotes)) (drop 1  $ dirtyVotes rawVotes) 

dirtyVotes :: String -> [[String]]
dirtyVotes rawVotes = tail (map (splitOn ",") (splitOn "\n" rawVotes))