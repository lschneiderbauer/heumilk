module Main where

import Heumilk.Fitness ( optimize1 )
import Heumilk.State ( rdInitialState )

main :: IO ()
main = print $ optimize1 (rdInitialState 123123 50)