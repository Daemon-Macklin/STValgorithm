module CleanData.CleanData where

import Data.List.Split (splitOn)
import Debug.Trace
import Data.List


type Candidate = (String, [Vote])
type Result = (String, Double)
type Vote = ([String], Double)

-- Starting weight for votes
weight :: Double
weight = 1000

-- Function to convert votes from String to Int
toInt :: String -> Int
toInt string = read string::Int

-- Function to get a list of candidates 
getCandidates :: String -> [Candidate]
getCandidates rawVotes = [(x, []) | x <- drop 2 (head $ dirtyVotes rawVotes)]

-- Function to calculate the quota
getQuota :: String -> String -> Int
getQuota rawVotes seats = (((length $ formatVotes rawVotes) * 1000) `div` ((toInt seats) + 1)) + 1000

-- Function to format votes
formatVotes :: String -> [Vote]
formatVotes rawVotes = filter ((/= []).fst) (map (formatVote) (checkInvalidVotes rawVotes))

-- Function to formate a vote
formatVote :: [(String, String)] -> Vote
formatVote vote = ([fst x | x <- vote, snd x /= "*"], weight)

-- Function to check all votes to see if invalid
checkInvalidVotes :: String -> [[(String, String)]]
checkInvalidVotes rawVotes = map checkInvalid (map removeBlanks (sortVotes rawVotes))

-- Function to remove all blank votes
removeBlanks :: [(String, String)] -> [(String, String)]
removeBlanks vote = ([x | x <- vote, snd x /= ""])

-- Function to check if vote is valid
checkInvalid :: [(String, String)] -> [(String, String)]
checkInvalid ballot
                    | x == 1 = if y /= 1 then (removeInvalid ballot) else ballot
                    | x == 2 = if y /= 3 then (removeInvalid ballot)  else ballot
                    | x == 3 = if y /= 6 then (removeInvalid ballot)  else ballot
                    | x == 4 = if y /= 10 then (removeInvalid ballot)  else ballot
                    | x == 5 = if y /= 15 then (removeInvalid ballot)  else ballot
                    | otherwise = []
                    where
                        x = length $ filter ((/= "*").snd) ballot
                        y = sum $ map (toInt . snd) (filter ((/= "*").snd) ballot)

-- Function to clean invalid votes
removeInvalid :: [(String, String)] -> [(String, String)]
removeInvalid ballot = isort $ checkGaps $ isort $ checkDups $ isort $ checkNumbers ballot 

-- Function to check if all numbers in ballot are valid
checkNumbers :: [(String, String)] -> [(String, String)]
checkNumbers ballot = (filter ((== "*").snd) ballot) ++  map checkNumber (filter ((/= "*").snd) ballot)

-- Function to check if number is valid in vote
checkNumber :: (String, String) -> (String, String)
checkNumber vote = 
                if (elem ((toInt . snd) vote) [1..5]) then vote
                else (fst vote, "*")

-- Function to check for duplicates in all ballots
checkDups :: [(String, String)] -> [(String, String)]
checkDups ballot = (filter ((== "*").snd) ballot) ++  checkDup (filter ((/= "*").snd) ballot)

-- Function to check for dups in vote
checkDup :: [(String, String)] -> [(String, String)]
checkDup ballot = head (groupBy (\a b -> ((toInt . snd) a)+1 == ((toInt . snd)b)) ballot)

-- FUnction to check if gaps in all ballots
checkGaps :: [(String, String)] -> [(String, String)]
checkGaps ballot = (filter ((== "*").snd) ballot) ++ concat ([x | x <- votes, length x == 1]) 
            where votes = checkGap (filter ((/= "*").snd) ballot)

-- Funciton to check if gaps in vote
checkGap :: [(String, String)] -> [[(String, String)]]
checkGap ballot = (groupBy (\a b -> ((toInt . snd) a) == ((toInt . snd)b)) ballot)

-- Function to sort all ballots
sortVotes :: String -> [[(String, String)]]
sortVotes rawVotes = map (isort) (cleanVotes rawVotes) 

-- Function to sort ballots to rank candidates in order of preference
isort :: Ord a => [(String, a)] -> [(String, a)]
isort [] = []
isort (x:xs) = insertionSort x (isort xs)

insertionSort :: Ord a => (String, a) -> [(String, a)]  -> [(String, a)]
insertionSort x [] = [x]
insertionSort x (y:ys) 
                | snd x <= snd y = x : y : ys
                | otherwise = y: insertionSort x ys

-- Function to combine a ballot to add the candidates names
zipVote :: [String] -> [String] -> [(String, String)]
zipVote candidates vote = zip (drop 2 candidates) (drop 2 vote)

-- Function to get the votes from the raw data
cleanVotes :: String -> [[(String, String)]]
cleanVotes rawVotes = map (zipVote (head $ dirtyVotes rawVotes)) (drop 1  $ dirtyVotes rawVotes) 

-- Function to format data from file to lists
dirtyVotes :: String -> [[String]]
dirtyVotes rawVotes = tail (map (splitOn ",") (splitOn "\n" rawVotes))