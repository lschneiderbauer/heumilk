module Main where

import Criterion.Main
import Heumilk.Fitness
import Heumilk.State ( rdInitialState )

main =
  defaultMain [
    bgroup
        "optimize1"
        [ bench "10" $ whnf optimize1 (rdInitialState 123 20),
          bench "20" $ whnf optimize1 (rdInitialState 123 20),
          bench "30" $ whnf optimize1 (rdInitialState 123 30),
          bench "40" $ whnf optimize1 (rdInitialState 123 40)
        ]
    , bgroup
        "cost"
        [ bench "20" $ whnf cost (rdInitialState 123 20)
        , bench "100" $ whnf cost (rdInitialState 123 100)
        , bench "200" $ whnf cost (rdInitialState 123 200)
        ]
    ]