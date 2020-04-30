 module STV.StvVote where

import CleanData.CleanData
import Debug.Trace

newWinners :: [Candidate]
newWinners = []

start :: String -> [Vote] -> [Candidate] -> Int -> [(String, Double)]
start seats votes candidates quota = map getCount (mainFunction (toInt seats) quota 0 candidates votes "count" newWinners)

mainFunction :: Int -> Int -> Int -> [Candidate] -> [Vote] -> String -> [Candidate] -> [Candidate]
mainFunction numOfSeats quota seatsFilled cans votes cycle elected
                                    | numOfSeats == seatsFilled = elected
                                    | cycle == "winners" = trace(show cycle ++ show (map getCount cans) ++ show (map getCount elected)) mainFunction numOfSeats quota (seatsFilled + 1) cans (updateWeights(winnersVotes) quota) "winnersCount" winners
                                    | cycle == "losers" = trace(show cycle ++ show (map getCount cans) ++ show (map getCount elected)) mainFunction numOfSeats quota seatsFilled (removeCandidate cans elected) (lastCandidateVotes cans) "losersCount" elected 
                                    | cycle == "winnersCount" = trace(show cycle ++ show (map getCount cans) ++ show (map getCount elected)) mainFunction numOfSeats quota seatsFilled currentCans [] "losers" elected
                                    | cycle == "losersCount" = trace(show cycle ++ show (map getCount cans) ++ show (map getCount elected)) mainFunction numOfSeats quota seatsFilled currentCans [] "winners" elected
                                    | seatsFilled == 0 = trace(show cycle ++ show (map getCount cans) ++ show (map getCount winners)) mainFunction numOfSeats quota seatsFilled firstCans [] "winners" elected
                                    where currentCans = allocateVotes (recycleVotes elected votes) cans
                                          firstCans = allocateVotes votes cans
                                          winners = findwinners cans quota elected
                                          winnersVotes = findwinnersVotes cans quota elected

removeCandidate :: [Candidate] -> [Candidate] -> [Candidate]
removeCandidate cans elected 
                      | length x == 0 = []
                      | length x == 1 = x
                      | otherwise = rankCandidates ( tail x)
                      where x = reverse (rankCandidates(removeElectedCans cans elected))

removeElectedCans :: [Candidate] -> [Candidate] -> [Candidate]
removeElectedCans cans elected = rankCandidates([x | x <- cans, (elem x elected) == False])


lastCandidateVotes :: [Candidate] -> [Vote]
lastCandidateVotes cans
                      | length x == 0 = []
                      | otherwise = (snd . head) x
                      where x = reverse (rankCandidates(cans))

--findlosers :: [Candidate] -> Int -> [Candidate]
--findlosers cans seats = rankCandidates(filter ((\x  -> (getValue x) < realToFrac(quota seats)).snd) cans) 

findwinners :: [Candidate] -> Int -> [Candidate] -> [Candidate]
findwinners cans quota winners  
                        | length x == 0 = ([head(rankCandidates(cans))] ++ winners)
                        | otherwise = (rankCandidates(take 1 x) ++ winners)
                        where x = filter ((\x  -> (getValue x) > realToFrac(quota)).snd) cans

findwinnersVotes :: [Candidate] -> Int -> [Candidate] -> [Vote]
findwinnersVotes cans quota winners  
                        | length x == 0 = (snd . head) $ rankCandidates(cans)
                        | otherwise = (snd . head) $ rankCandidates(x)
                        where x = filter ((\x  -> (getValue x) > realToFrac(quota)).snd) cans

recycleVotes :: [Candidate] -> [Vote] -> [Vote]
recycleVotes elected votes = filter ((/= []).fst) $ map (recycleVote elected) votes

recycleVote :: [Candidate] -> Vote -> Vote 
recycleVote elected vote = ((findValidVote (map fst elected) 1 (fst vote), snd vote))

findValidVote :: [String] -> Int -> [String] -> [String]
findValidVote elected index vote 
                              | (drop index vote) == [] = drop index vote
                              | elem (head (drop index vote)) elected == True = findValidVote elected (index + 1) vote
                              | otherwise = drop index vote

updateWeights :: [Vote] -> Int -> [Vote]
updateWeights votes quota = trace(show (getValue votes) ++ " -> " ++ show (getValue (map (updateWeight ((getValue votes)) quota) votes ))) map (updateWeight ((getValue votes)) quota) votes 

updateWeight :: Double -> Int -> Vote -> Vote
updateWeight totalWeight quota vote
                                          | x <= y = (fst vote, snd vote)
                                          | otherwise = (fst vote, snd vote * (y/x))
                                          where
                                                x = realToFrac totalWeight
                                                y = totalWeight - (realToFrac (quota))

-- (fst vote, (snd vote * (realToFrac(total - (quota)) / realToFrac(totalWeight))))
rank :: [Vote] -> [Candidate]  -> [Result]
rank xs cans = map getCount (allocateVotes xs cans)

getCount :: Candidate -> Result
getCount candidate = (fst candidate, getValue (snd candidate)) 

getValue :: [Vote] -> Double
getValue votes = sum [(snd x) | x <- votes]

allocateVotes :: [Vote] -> [Candidate] -> [Candidate]
allocateVotes xs cans = rankCandidates(map (allocateVote xs) cans)

allocateVote:: [Vote] -> Candidate -> Candidate
allocateVote votes candidate = (fst candidate, snd candidate ++ filter (\ x -> head (fst x) == (fst candidate)) (filter ((/= []).fst)  votes))

rankCandidates :: [Candidate] -> [Candidate]
rankCandidates [] = []
rankCandidates (x:xs) = candidateSort x (rankCandidates xs)

candidateSort :: Candidate -> [Candidate] -> [Candidate]
candidateSort x [] = [x]
candidateSort x (y:ys)
                | getValue (snd x) >= getValue (snd y) = x : y : ys
                | otherwise = y : candidateSort x ys