{-# LANGUAGE FlexibleInstances #-}
module Heumilk.State where

import Heumilk.Network

import Data.Maybe
import Test.QuickCheck

import System.Random

import qualified Data.Vector as V
import qualified Data.Matrix.Unboxed as M
import qualified Data.Bifunctor

siteList n = Origin : (Site <$> [1..n])

instance Arbitrary Site where
  arbitrary = elements $ siteList 3

data Position = Position { x :: Float, y :: Float } deriving (Show, Eq)
instance Arbitrary Position where
  arbitrary = do
    x <- choose (-100, 100)
    y <- choose (-100, 100)
    return $ Position x y
instance Random Position where
  random g0 = (Position x y, g2)
    where
      (x, g1) = random g0
      (y, g2) = random g1     
  randomR (p1, p2) g0 = (Position cx cy, g2)
    where
      (cx, g1) = randomR (x p1, x p2) g0
      (cy, g2) = randomR (y p1, y p2) g1     

instance Arbitrary PN where
  arbitrary = do
    n_sites <- chooseInt (1, 10)
    demands <- vectorOf n_sites $ chooseInt (0, 33)
    site_positions <- vectorOf (n_sites + 1) (arbitrary :: (Gen Position))
    return $ createInitialState (V.fromList $ fromIntegral <$> demands) (distanceMat site_positions)

demand :: Int -> Demand
demand n_sites = V.fromList $ take n_sites $ fromIntegral <$> randomRs (0 :: Int , 33) (mkStdGen 123123)


-- just to help creating the distance matrix
pos :: Int -> [Position]
pos n = take n $ randomRs (Position (-100) 100, Position 100 100) (mkStdGen 123123)

distf :: Floating a => (a, a) -> (a, a) -> a
distf (x1, y1) (x2, y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

distanceMat :: [Position] -> DistanceMatrix Float
distanceMat poss = M.fromList (l, l) (distf <$> pos <*> pos) where
  pos = zip (x <$> poss) (y <$> poss)
  l = length poss

initialState :: Int -> PN
initialState n = createInitialState (demand n) (distanceMat $ pos (n+1))

{- 
initialState = netFromList $ directRoute <$> filter demandExists siteList
  where
    directRoute site = MkPalletRoute [site] (demand site)
    demandExists site = demand site > 0

sampleState =
  netFromList
    [ MkPalletRoute [Site 3, Site 2, Site 1] 20
    , MkPalletRoute [Site 2, Site 1] 20
    ] -}
