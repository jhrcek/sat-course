{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude hiding (and, not, or)
import SMTLib2 (Command (CmdAssert, CmdCheckSat, CmdDeclareFun), Expr (Lit),
                Literal (LitNum), Script (Script), app, pp)
import SMTLib2.Core (not, or, tBool)
import SMTLib2.Int (tInt)
import System.Process (readProcess)

main :: IO ()
main = do
    let script = show (pp $ nQueensScript 4)
                -- hack, because SMTLib2 doesn't seem to support get-model command
                ++ "\n(get-model)"
    writeFile "queens" script
    putStrLn =<< readProcess "z3" ["-smt2", "queens"] []

nQueensScript :: Integer -> Script
nQueensScript n = Script
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
    indices = [1..n]
    atLeastOneInRow r = app "or" [ p r c | c <- indices ]
    atLeastOneInEachRow = [ atLeastOneInRow r | r <- indices ]

    atLeastOneInColumn c = app "or" [ p r c | r <- indices ]
    atLeastOneInEachColumn = [ atLeastOneInColumn c | c <- indices ]

    atMostOneInRow r = [ not (p r j) `or` not (p r k) | j <- indices, k <- [j+1..n] ]
    atMostOneInEachRow = concat [atMostOneInRow r | r <- indices]

    atMostOneInColumn c = [ not (p j c) `or` not (p k c) | j <- indices, k <- [j+1..n] ]
    atMostOneInEachColumn = concat [atMostOneInColumn c | c <- indices]

    atMostOneInEachDiagonal = [ not (p i j) `or` not (p i' j')
        | i  <- indices
        , i' <- [i+1..8]
        , j  <- indices
        , j' <- indices
        , i+j == i'+j' || i-j == i'-j'
        ]
