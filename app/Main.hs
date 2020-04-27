module Main where

import Lib
import Alternate.AltVote
import STV.StvVote
import CleanData.CleanData

import Text.Read

main :: IO ()
main = do

    rawVotes <- readFile "votes.csv"
    print "Seats for STV?"
    seats <- getLine

    -- print $ rawVotes

    let candidates = (getCandidates rawVotes)
    let quota = (getQuota rawVotes seats) 
    let votes = (formatVotes rawVotes)

    print "Number of Votes"
    print $ length votes
    print "Candidates:"
    print $ candidates
    print "========================"

    print "Alternative Vote Results:"
    print $ startAltVote votes 
    print "========================"
    
    print "STV Quota"
    print $ quota
    -- print $ votes
    print "Single Transferable Vote Results:"
    print $ start seats votes candidates quota 
