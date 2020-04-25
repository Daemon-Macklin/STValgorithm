module STV.StvVote where

import CleanData.CleanData

winners :: [Candidate]
winners = []

start :: String -> [(String, Double)]
start seats = map getCount (mainFunction (toInt seats) 0 candidates formatVotes "count" winners)

toInt :: String -> Int
toInt string = read string::Int

mainFunction :: Int -> Int -> [Candidate] -> [Vote] -> String -> [Candidate] -> [Candidate]
mainFunction numOfSeats seatsFilled cans votes cycle elected
                                    | numOfSeats == seatsFilled = elected
                                    | cycle == "winners" = mainFunction numOfSeats (seatsFilled) cans (updateWeights (snd (head winners)) numOfSeats) "losers" winners
                                    | cycle == "losers" = mainFunction numOfSeats (seatsFilled) (removeCandidate cans elected) ((updateWeights(lastCandidateVotes cans) numOfSeats) ++ votes) "count" elected 
                                    | seatsFilled == 0 = mainFunction numOfSeats (seatsFilled + 1) firstCans votes "winners" elected
                                    | otherwise = mainFunction numOfSeats (seatsFilled + 1) currentCans newVotes "winners" elected
                                    where newVotes = snd (head currentCans)
                                          currentCans = allocateVotes (map recycleVote votes) cans
                                          firstCans = allocateVotes votes cans
                                          winners = findwinners cans numOfSeats elected

removeCandidate :: [Candidate] -> [Candidate] -> [Candidate]
removeCandidate cans elected 
                      | length x == 0 = []
                      | length x == 1 = x
                      | otherwise = rankCandidates ( tail x)
                      where x = reverse (rankCandidates(removeElectedCans cans elected))

removeElectedCans :: [Candidate] -> [Candidate] -> [Candidate]
removeElectedCans cans elected = rankCandidates([x | x <- cans, (elem x elected) == False])


lastCandidateVotes :: [Candidate] -> [Vote]
lastCandidateVotes cans = snd (head( rankCandidates(tail (reverse cans))))

findlosers :: [Candidate] -> Int -> [Candidate]
findlosers cans seats = rankCandidates(filter ((\x  -> (getValue x) < realToFrac(quota seats)).snd) cans) 

findwinners :: [Candidate] -> Int -> [Candidate] -> [Candidate]
findwinners cans seats winners  
                        | length x == 0 = rankCandidates([head(rankCandidates(cans))] ++ winners)
                        | otherwise = rankCandidates(x ++ winners)
                        where x = (filter ((\x  -> (getValue x) > realToFrac(quota seats)).snd) cans)

recycleVote :: Vote -> Vote 
recycleVote vote = ((drop 1 (fst vote), snd vote))

updateWeights :: [Vote] -> Int -> [Vote]
updateWeights votes seats = map (updateWeight ((getValue votes) * 1000) (length votes) seats) votes 

updateWeight :: Double -> Int -> Int -> Vote -> Vote
updateWeight totalWeight total seats vote = (fst vote, (snd vote * (realToFrac(((total - (quota seats)) * 1000)) / realToFrac(totalWeight))))
                                        
rank :: [Vote] -> [Candidate]  -> [Result]
rank xs cans = map getCount (allocateVotes xs cans)

getCount :: Candidate -> Result
getCount candidate = (fst candidate, getValue (snd candidate)) 

getValue :: [Vote] -> Double
getValue votes = sum [(snd x) / 1000 | x <- votes]

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