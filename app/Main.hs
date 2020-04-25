module Main where

import Lib
import Alternate.AltVote
import STV.StvVote
import CleanData.CleanData

import Text.Read

main :: IO ()
main = do

    print "Alternative Vote Results:"
    print $ startAltVote formatVotes
    print "========================"
    print "Seats for STV?"
    seats <- getLine
    print "Single Transferable Vote Results:"
    print $ start seats
