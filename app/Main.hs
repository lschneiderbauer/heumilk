module Main where

import Heumilk.State
import NetworkPlot

main :: IO ()
main = do
  id <- plotPN (initialState 5)
  print id