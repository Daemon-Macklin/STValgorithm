module CleanData.CleanData where

import Data.List.Split (splitOn)
import Debug.Trace


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
getQuota rawVotes seats = (((length $ formatVotes rawVotes) * 1000) `div` ((toInt seats) + 1)) + 1000

formatVotes :: String -> [Vote]
formatVotes rawVotes = filter ((/= []).fst) (map (formatVote) (removeInvalidVotes rawVotes))

formatVote :: [(String, String)] -> Vote
formatVote vote = ([fst x | x <- vote, snd x /= "*"  && snd x /= ""], weight)

removeInvalidVotes :: String -> [[(String, String)]]
removeInvalidVotes rawVotes = map removeInvalid (sortVotes rawVotes)

removeInvalid :: [(String, String)] -> [(String, String)]
removeInvalid ballot
                    | x == 1 = if y /= 1 then [] else ballot
                    | x == 2 = if y /= 3 then [] else ballot
                    | x == 3 = if y /= 6 then [] else ballot
                    | x == 4 = if y /= 10 then [] else ballot
                    | x == 5 = if y /= 15 then [] else ballot
                    | otherwise = []
                    where
                        x = length $ filter ((/= "*").snd) ballot
                        y = sum $ map (toInt . snd) (filter ((/= "*").snd) ballot)

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