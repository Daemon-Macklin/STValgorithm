module CleanData.CleanData where

import Data.List.Split (splitOn)
import Debug.Trace
import Data.List


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
formatVotes rawVotes = filter ((/= []).fst) (map (formatVote) (checkInvalidVotes rawVotes))

formatVote :: [(String, String)] -> Vote
formatVote vote = ([fst x | x <- vote, snd x /= "*"], weight)

checkInvalidVotes :: String -> [[(String, String)]]
checkInvalidVotes rawVotes = map checkInvalid (map removeBlanks (sortVotes rawVotes))

removeBlanks :: [(String, String)] -> [(String, String)]
removeBlanks vote = ([x | x <- vote, snd x /= ""])

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

removeInvalid :: [(String, String)] -> [(String, String)]
removeInvalid ballot = isort $ checkGaps $ isort $ checkDups $ isort $ checkNumbers ballot 

checkNumbers :: [(String, String)] -> [(String, String)]
checkNumbers ballot = (filter ((== "*").snd) ballot) ++  map checkNumber (filter ((/= "*").snd) ballot)

checkNumber :: (String, String) -> (String, String)
checkNumber vote = 
                if (elem ((toInt . snd) vote) [1..5]) then vote
                else (fst vote, "*")

checkDups :: [(String, String)] -> [(String, String)]
checkDups ballot = (filter ((== "*").snd) ballot) ++  checkDup (filter ((/= "*").snd) ballot)

checkDup :: [(String, String)] -> [(String, String)]
checkDup ballot = head (groupBy (\a b -> ((toInt . snd) a)+1 == ((toInt . snd)b)) ballot)

checkGaps :: [(String, String)] -> [(String, String)]
checkGaps ballot = trace(show( [x | x <- votes, length x == 1] )) (filter ((== "*").snd) ballot) ++ [x | x <- votes, length x == 1] 
            where votes = checkDup (filter ((/= "*").snd) ballot)

checkGap :: [(String, String)] -> [[(String, String)]]
checkGap ballot = (groupBy (\a b -> ((toInt . snd) a) == ((toInt . snd)b)) ballot)

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