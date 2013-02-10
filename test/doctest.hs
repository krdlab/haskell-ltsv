module Main where

import Test.DocTest

main :: IO ()
main = doctest
        [ "-XOverloadedStrings"
        , "Data/LTSV.hs"
        , "Data/LTSV/String.hs"
        ]

