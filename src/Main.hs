{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (and, not, or)
import SMTLib2
import SMTLib2.Core
import SMTLib2.Int
import System.Process (readProcess)

main :: IO ()
main = do

    let script = show (pp eightQueensScript)
                -- hack, because SMTLib2 doesn't seem to support get-model command
                ++ "\n(get-model)"
    writeFile "queens" script
    putStrLn =<< readProcess "z3" ["-smt2", "queens"] []

eightQueensScript :: Script
eightQueensScript = Script
    [ CmdDeclareFun "p" [tInt, tInt] tBool
    , CmdAssert $ app "and" $
        atLeastOneInEachRow <>
        atMostOneInEachRow <>
        atLeastOneInEachColumn <>
        atMostOneInEachColumn <>
        atMostOneInEachDiagonal
    , CmdCheckSat
    ]
  where
    p i j = app "p" [Lit (LitNum i) , Lit (LitNum j)]
    indices = [1..8]
    atLeastOneInRow r = app "or" [ p r c | c <- indices ]
    atLeastOneInEachRow = [ atLeastOneInRow r | r <- indices ]

    atLeastOneInColumn c = app "or" [ p r c | r <- indices ]
    atLeastOneInEachColumn = [ atLeastOneInColumn c | c <- indices ]

    atMostOneInRow r = [ not (p r j) `or` not (p r k) | j <- indices, k <- [j+1..8] ]
    atMostOneInEachRow = concat [atMostOneInRow r | r <- indices]

    atMostOneInColumn c = [ not (p j c) `or` not (p k c) | j <- indices, k <- [j+1..8] ]
    atMostOneInEachColumn = concat [atMostOneInColumn c | c <- indices]

    atMostOneInEachDiagonal = [ not (p i j) `or` not (p i' j')
        | i  <- indices
        , i' <- [i+1..8]
        , j  <- indices
        , j' <- indices
        , i+j == i'+j' || i-j == i'-j'
        ]
