 module STV.StvVote where

import CleanData.CleanData
import Debug.Trace

newWinners :: [Candidate]
newWinners = []

start :: String -> [Vote] -> [Candidate] -> Int -> [(String, Double)]
start seats votes candidates quota = map getCount (mainFunction (toInt seats) quota 0 candidates votes "count" newWinners)

mainFunction :: Int -> Int -> Int -> [Candidate] -> [Vote] -> String -> [Candidate] -> [Candidate]
mainFunction numOfSeats quota seatsFilled cans votes cycle elected
                                    | numOfSeats == seatsFilled = verifyResult elected cans
                                    | cycle == "winners" = mainFunction numOfSeats quota (seatsFilled + 1) (removeElectedCans cans winners) (updateWeights(winnersVotes) quota) "winnersCount" winners
                                    | cycle == "losers" = mainFunction numOfSeats quota seatsFilled (removeCandidate cans elected) (lastCandidateVotes cans) "losersCount" elected 
                                    | cycle == "winnersCount" = mainFunction numOfSeats quota seatsFilled currentCans [] (overQuota cans quota) elected
                                    | cycle == "losersCount" = mainFunction numOfSeats quota seatsFilled currentCans [] "winners" elected
                                    | seatsFilled == 0 = mainFunction numOfSeats quota seatsFilled firstCans [] "winners" elected
                                    where currentCans = allocateVotes (recycleVotes cans votes) cans
                                          firstCans = allocateVotes votes cans
                                          winners = findwinners cans quota elected
                                          winnersVotes = findwinnersVotes cans quota elected

-- trace("Cycle:" ++ show cycle ++ "\n Current Candidates" ++ show (map getCount cans) ++ "\n Current Winners" ++ show (map getCount elected) ++ "\n")

verifyResult :: [Candidate] -> [Candidate] -> [Candidate]
verifyResult elected cans 
                        | length cans == 0 = reverse elected
                        | otherwise = allocateVotes (recycleVotes ([head elected]) ((snd . head) cans)) elected

overQuota :: [Candidate] -> Int -> String
overQuota cans quota  
                  | x == [] = "losers"
                  | otherwise = "winners"
                  where x = filter ((\x  -> (getValue x) >= realToFrac(quota)).snd) cans

removeCandidate :: [Candidate] -> [Candidate] -> [Candidate]
removeCandidate cans elected 
                      | length x == 0 = []
                      | length x == 1 = x
                      | otherwise = trace(show ((fst . head) x) ++ "Eleminated") rankCandidates ( tail x)
                      where x = reverse (rankCandidates(removeElectedCans cans elected))

removeElectedCans :: [Candidate] -> [Candidate] -> [Candidate]
removeElectedCans cans elected = rankCandidates([x | x <- cans, (elem x elected) == False])

lastCandidateVotes :: [Candidate] -> [Vote]
lastCandidateVotes cans
                      | length x == 0 = []
                      | otherwise = (snd . head) x
                      where x = reverse (rankCandidates(cans))

findwinners :: [Candidate] -> Int -> [Candidate] -> [Candidate]
findwinners cans quota winners  
                        | length x == 0 = trace(show ( fst (head (rankCandidates(cans))) ) ++ " Elected") ([head(rankCandidates(cans))] ++ winners)
                        | otherwise = trace(show ( fst (head (rankCandidates(take 1 x))) ) ++ " Elected") (rankCandidates(take 1 x) ++ winners)
                        where x = filter ((\x  -> (getValue x) >= realToFrac(quota)).snd) cans

findwinnersVotes :: [Candidate] -> Int -> [Candidate] -> [Vote]
findwinnersVotes cans quota winners  
                        | length x == 0 = (snd . head) $ rankCandidates(cans)
                        | otherwise = (snd . head) $ rankCandidates(x)
                        where x = filter ((\x  -> (getValue x) >= realToFrac(quota)).snd) cans

recycleVotes :: [Candidate] -> [Vote] -> [Vote]
recycleVotes cans votes = filter ((/= []).fst) $ map (recycleVote cans) votes

recycleVote :: [Candidate] -> Vote -> Vote 
recycleVote cans vote = ((findValidVote (map fst cans) 1 (fst vote), snd vote))

findValidVote :: [String] -> Int -> [String] -> [String]
findValidVote cans index vote 
                              | (drop index vote) == [] = drop index vote
                              | elem (head (drop index vote)) cans == False = findValidVote cans (index + 1) vote
                              | otherwise = drop index vote

updateWeights :: [Vote] -> Int -> [Vote]
updateWeights votes quota = trace ("New Weight Factor: " ++ show (calcWeightFactor votes quota))  map (\ x -> (fst x, (snd x * (calcWeightFactor votes quota)))) votes 

calcWeightFactor:: [Vote] -> Int -> Double
calcWeightFactor votes quota
                        | x <= y = 1
                        | otherwise = y / x
                        where
                              x = realToFrac (getValue votes)
                              y = x - (realToFrac (quota))

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