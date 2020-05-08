 module STV.StvVote where

import CleanData.CleanData
import Debug.Trace

newWinners :: [Candidate]
newWinners = []

-- Start function used to enter the mainFunction loop
start :: String -> [Vote] -> [Candidate] -> Int -> [(String, Double)]
start seats votes candidates quota = map getCount (mainFunction (toInt seats) quota 0 candidates votes "count" newWinners)

-- Recursive Function used to run the election cycles
mainFunction :: Int -> Int -> Int -> [Candidate] -> [Vote] -> String -> [Candidate] -> [Candidate]
mainFunction numOfSeats quota seatsFilled cans votes cycle elected
                                    | numOfSeats == seatsFilled = elected 
                                    | cycle == "winners" = mainFunction numOfSeats quota (seatsFilled + 1) (removeElectedCans cans winners) (updateWeights (removeElectedCans cans winners) (winnersVotes) quota) "winnersCount" winners
                                    | cycle == "losers" = mainFunction numOfSeats quota seatsFilled (removeCandidate cans elected) (lastCandidateVotes cans) "losersCount" elected 
                                    | cycle == "winnersCount" =  mainFunction numOfSeats quota seatsFilled currentCans [] (overQuota currentCans quota) elected
                                    | cycle == "losersCount" = mainFunction numOfSeats quota seatsFilled currentCans [] "winners" elected
                                    | seatsFilled == 0 = mainFunction numOfSeats quota seatsFilled firstCans [] "winners" elected
                                    where currentCans = allocateVotes (recycleVotes cans votes) cans
                                          firstCans = allocateVotes votes cans
                                          winners = findwinners cans quota elected
                                          winnersVotes = findwinnersVotes cans quota elected

-- Useful Trace for debuggin mainFunction
-- trace("Cycle:" ++ show cycle ++ "\n Current Candidates" ++ show (map getCount cans) ++ "\n Current Winners" ++ show (map getCount elected) ++ "\n")

unelected :: [Candidate] -> [(String, Double)] -> [String] 
unelected candidates elected = [fst x | x <- candidates, (fst x) `notElem` (map fst elected)]  

-- Function to check if anyone is over the quota to decide the next cycle
overQuota :: [Candidate] -> Int -> String
overQuota cans quota  
                  | x == [] = "losers"
                  | otherwise = "winners"
                  where x = filter ((\ y  -> (getValue y) >= realToFrac(quota)).snd) cans

-- Function to eliminate the lowest candidate
removeCandidate :: [Candidate] -> [Candidate] -> [Candidate]
removeCandidate cans elected 
                      | length x == 0 = []
                      | length x == 1 = x
                      | otherwise = trace(show ((fst . head) x) ++ "Eleminated") rankCandidates ( tail x)
                      where x = reverse (rankCandidates(removeElectedCans cans elected))

-- Function to remove elected candidates
removeElectedCans :: [Candidate] -> [Candidate] -> [Candidate]
removeElectedCans cans elected = rankCandidates([x | x <- cans, (elem x elected) == False])

-- Function to find the loweset votes candidates votes
lastCandidateVotes :: [Candidate] -> [Vote]
lastCandidateVotes cans
                      | length x == 0 = []
                      | otherwise = (snd . head) x
                      where x = reverse (rankCandidates(cans))

-- Function to find the winner
findwinners :: [Candidate] -> Int -> [Candidate] -> [Candidate]
findwinners cans quota winners  
                        | length x == 0 = trace(show ( fst (head (rankCandidates(cans))) ) ++ " Elected") ([head(rankCandidates(cans))] ++ winners)
                        | otherwise = trace(show ( fst (head (rankCandidates(take 1 x))) ) ++ " Elected") (rankCandidates(take 1 x) ++ winners)
                        where x = filter ((\x  -> (getValue x) >= realToFrac(quota)).snd) cans

-- Function to find the winners votes
findwinnersVotes :: [Candidate] -> Int -> [Candidate] -> [Vote]
findwinnersVotes cans quota winners  
                        | length x == 0 = (snd . head) $ rankCandidates(cans)
                        | otherwise = (snd . head) $ rankCandidates(x)
                        where x = filter ((\x  -> (getValue x) >= realToFrac(quota)).snd) cans

-- Function to recycle the votes
recycleVotes :: [Candidate] -> [Vote] -> [Vote]
recycleVotes cans votes = filter ((/= []).fst) $ map (recycleVote cans) votes

-- Function to recycle one vote
recycleVote :: [Candidate] -> Vote -> Vote 
recycleVote cans vote = ((findValidVote (map fst cans) 0 (fst vote), snd vote))

-- Function to find the next valid vote on a ballot
findValidVote :: [String] -> Int -> [String] -> [String]
findValidVote cans index vote 
                              | (drop index vote) == [] = drop index vote
                              | elem (head (drop index vote)) cans == False = findValidVote cans (index + 1) vote
                              | otherwise = drop index vote

-- Function to remove-nontransferable weights then update weight of ballots
updateWeights :: [Candidate] -> [Vote] -> Int -> [Vote]
updateWeights candidates votes quota = trace("Transferable Votes:" ++ show (length(recycleVotes candidates votes))) updateWeight (recycleVotes candidates votes) quota (getValue votes)

-- Function to update all of the weights
updateWeight :: [Vote] -> Int -> Double -> [Vote]
updateWeight votes quota total = trace ("New Weight Factor: " ++ show (calcWeightFactor votes quota total) ++ "\nSurplus: " ++ show ((total) - (realToFrac quota)) ++ "\n")  map (\ x -> (fst x, (snd x * (calcWeightFactor votes quota total)))) votes 

-- Funtion to caclulate the new weight factor
calcWeightFactor:: [Vote] -> Int -> Double -> Double
calcWeightFactor votes quota total
                        | x <= y = 1
                        | otherwise = y / x
                        where
                              x = realToFrac (getValue votes)
                              y = total - (realToFrac (quota))

-- Function to rank the candidates
rank :: [Vote] -> [Candidate]  -> [Result]
rank xs cans = map getCount (allocateVotes xs cans)

-- Function to tuple of candidates and their vote weight
getCount :: Candidate -> Result
getCount candidate = (fst candidate, getValue (snd candidate)) 

-- Function to calculate the sum of a candidates vote
getValue :: [Vote] -> Double
getValue votes = sum [(snd x) | x <- votes]

-- Function to allocate votes to candidates
allocateVotes :: [Vote] -> [Candidate] -> [Candidate]
allocateVotes xs cans = rankCandidates(map (allocateVote xs) cans)

-- Function to allocate votes to a candidate
allocateVote:: [Vote] -> Candidate -> Candidate
allocateVote votes candidate = (fst candidate, snd candidate ++ filter (\ x -> head (fst x) == (fst candidate)) (filter ((/= []).fst)  votes))

-- Function to rank candidate
rankCandidates :: [Candidate] -> [Candidate]
rankCandidates [] = []
rankCandidates (x:xs) = candidateSort x (rankCandidates xs)

-- Function to rank candidate
candidateSort :: Candidate -> [Candidate] -> [Candidate]
candidateSort x [] = [x]
candidateSort x (y:ys)
                | getValue (snd x) >= getValue (snd y) = x : y : ys
                | otherwise = y : candidateSort x ys