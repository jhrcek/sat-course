{-# LANGUAGE OverloadedStrings #-}

module Main where

import SMTLib2
import SMTLib2.Core
import SMTLib2.Int

main :: IO ()
main =
  print $ pp eightQueensScript

eightQueensScript :: Script
eightQueensScript = Script
    [ CmdDeclareFun "f" [tInt, tInt] tBool
    ]
