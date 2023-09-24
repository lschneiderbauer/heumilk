{-# LANGUAGE FlexibleInstances #-}
module Heumilk.State where

import Heumilk.Network

import Test.QuickCheck
    ( Arbitrary(arbitrary),
      choose,
      chooseInt,
      elements,
      vectorOf,
      Gen )

import System.Random
    ( Random(randomRs, random, randomR), mkStdGen )

import qualified Data.Vector as V
import qualified Data.Matrix.Unboxed as M
import qualified Data.Sequence as S

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
    demands <- vectorOf n_sites $ chooseInt (1, 10)
    site_positions <- vectorOf (n_sites + 1) (arbitrary :: (Gen Position))
    return $ createInitialState (V.fromList $ fromIntegral <$> demands) (distanceMat site_positions)

rdDemand :: Int -> Int -> Demand
rdDemand seed n_sites =
  V.fromList $ take n_sites $ fromIntegral <$> randomRs (1 :: Int , 10) (mkStdGen seed)


-- just to help creating the distance matrix
rdPos :: Int -> Int -> [Position]
rdPos seed n = take n $ randomRs (Position (-100) 100, Position 100 100) (mkStdGen seed)

distf :: Floating a => (a, a) -> (a, a) -> a
distf (x1, y1) (x2, y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

distanceMat :: [Position] -> DistanceMatrix
distanceMat poss = M.fromList (l, l) (distf <$> pos <*> pos) where
  pos = zip (x <$> poss) (y <$> poss)
  l = length poss

rdInitialState :: Int -> Int -> PN
rdInitialState seed n = createInitialState (rdDemand seed n) (distanceMat $ rdPos seed (n+1))

type Demand = V.Vector NPallets

createInitialState :: Demand -> DistanceMatrix -> PN
createInitialState dem distMat =
  if 1 + V.length dem <= M.cols distMat then
    MkNetwork { routes = rs, distances = distMat }
  else
    error "The given distance matrix is not covering all sites with demands."
  where
    rs = V.toList $ V.imap to_pr (V.drop 0 dem)
    to_pr i dem = MkPalletRoute {prSites = S.singleton $ Site (i+1), pallets = dem }