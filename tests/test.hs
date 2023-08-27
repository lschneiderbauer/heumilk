module Main where

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
-- import Test.Tasty.Falsify

import qualified Data.Matrix.Unboxed as M

import Heumilk.Network
import Heumilk.State
import Heumilk.Fitness

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "distance is symmetric" $
      \ pn -> M.isSymmetric $ distances (pn :: PN)

  , QC.testProperty "tnFromPn preserves transport volume" $
      \ pn -> totalVolume pn == totalVolume (tnFromPn pn)
      {-
  , QC.testProperty "triangle inequality" $
      \x y z -> distance x z <= distance x y + distance y z
      -}
  ]


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT
{-
  , testCase "palletsOnSegments correct" $
      palletsOnSegment (Site 2, Site 1) initialState @?= 40.0

  , testCase "cost function correct" $
      cost initialState 3 @?= 209
-}
  ]