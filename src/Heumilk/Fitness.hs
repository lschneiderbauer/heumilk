{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}

module Heumilk.Fitness where

import Data.Function ( (&) )
import Data.List ( elemIndex, nubBy, sortOn, union )
import Data.List.Extra
    ( elemIndex, nubBy, sortOn, union, disjoint, minimumOn )
import Data.Foldable

import qualified Data.Sequence as S
import Data.Sequence ( Seq (..), (><) )

import Heumilk.Network



(|>) = (&)
infixl 3 |>


class Measurable a where
  cost :: a -> Float

instance Measurable TN where
  cost tn = sum $ tc_cost <$> routes tn
    where
      tc_cost tc = sum $ ts_cost <$> segments tc
      ts_cost = uncurry (distance $ distances tn)


instance Measurable PN where
  cost = cost . tnFromPn

{-
instance Ord TN where
  compare net1 net2 = cost net1 `compare` cost net2

instance Ord PN where
  compare :: PN -> PN -> Ordering
  compare net1 net2 = cost net1 `compare` cost net2
-}

-- we are using here that Data.Set uses an ordering defined by Ord
netSites :: PN -> [Site]
netSites net = foldr union [] (toList . prSites <$> routes net)

nearestSingletonLeafRoute :: Route a => Network a -> Site -> Maybe a
nearestSingletonLeafRoute net s
  | null filteredRoutes = Nothing
  | otherwise = Just $ minimumOn (distance (distances net) s . dest) filteredRoutes
  where
    filteredRoutes = filter pred (leafRoutes net)
    pred route = isSingleton route && dest route /= s

takeUntilEq :: Eq a => a -> [a] -> Maybe [a]
takeUntilEq until lst = elemIndex until lst >>= Just . (+ 1) >>= Just . (`take` lst)

absorbNext :: PN -> PalletRoute -> Maybe (PN, PalletRoute)
absorbNext pn r1 = ret =<< nearestSingletonLeafRoute pn (dest r1)
  where
    ret r2 =
      let r2_mod = r1 <> r2
      in Just (replacePRoute pn r2 (Just r2_mod), r2_mod)

greedyAbsorbtion :: PN -> PalletRoute -> (PN, PalletRoute)
greedyAbsorbtion pn pr =
  case absorbNext pn pr of
    Just (pn_new, pr_new) -> greedyAbsorbtion pn_new pr_new
    Nothing -> (pn, pr)


-- dumb strategy: put everything in one route.
optimize0 :: PN -> PN
optimize0 pn =
  case maybeFirstRoute of
    Just firstRoute -> fst $ greedyAbsorbtion pn firstRoute
    Nothing -> pn
  where
    maybeFirstRoute = nearestSingletonLeafRoute pn Origin

-----------------------------------------------
-- Clark & Wright variant

-- The saving will not only depend on the distances,
-- but also on whether an additional truck needs to be added or not.
-- we should take this into account!
distanceSaving :: DistanceMatrix -> Site -> Site -> Float
distanceSaving mat s1 s2 = dist Origin s2 + dist Origin s1 - dist s1 s2
  where
    dist = distance mat

-- cost difference on whether routes with those sites should
-- be joined, or not.
saving :: PN -> Site -> Site -> Float
saving pn s1 s2
  | s1 == s2 = 0
  | otherwise = cost subnet - cost subnet_alt
  where
    subnet = subNetwork pn [s1, s2]
    subnet_alt = netMod subnet s1 s2

netMod :: PN -> Site -> Site -> PN
netMod pn site1 site2 =
  case leafRouteWithSite pn site1 of
    Just baseRoute -> foldr (\a net -> uncurry (replacePRoute net) a) pn (zip routesToAbsorb (Just <$> newRoutes))
      where
        routesToAbsorb =
          routes pn |>
            filter (not . crossesRoute baseRoute) |>
            filter (flip isStart $ site2) -- && notElem site1 (prSites r))
        newRoutes = (baseRoute <>) <$> routesToAbsorb

        -- the confusing naming comes from the fact that we originally
        -- stored the lists of sites in reverse (head was "last" visit)
        isStart pr site =
          case prSites pr of
            htail :|> last -> site == last
            _ -> False

    Nothing -> pn


optimize1 :: PN -> PN
optimize1 pn =
  let
    sav = saving pn
    sites = netSites pn
    siteTuplesSortedBySaving =
      (,) <$> sites <*> sites |>
        filter (\t -> uncurry sav t > 0) |>
        sortOn (negate . uncurry sav) |>
        nubBy (\t1 t2 -> not $ disjoint [fst t1, snd t1] [fst t2, snd t2])
  in
    if null siteTuplesSortedBySaving then
      pn
    else
      optimize1 $ foldr (flip (uncurry . netMod)) pn siteTuplesSortedBySaving
      -- optimize1 $ foldl' (uncurry . netMod) pn siteTuplesSortedBySaving


-- start with the site farthest away from START
-- then absorb the nearest one (we might want to pick one that :is between the END and START)

-- edit: why should we not be conservative about aborption: i.e. only (partially) absorb the amount
-- that doesn't generate additional trucks
-- -> locally that would probably mean that we do not gain any benefit, since
--    the truck of the absorbed route still has to be there.
-- -> but globally it could be that the "other" part can also be absorbed by another route, and so we gain a benefit
--    .. this can probably not be captured in a local approach?
--  we might do a first run where we only capture what's locally considered good.
--  in a second run we might change strategy with remaining singleton routes, and compare the overall result?

-- when should we stop doing that?
-- (absorption can also be bad in case we already have a full truck to one site for example)
-- so might want to absorb in a way that we get single load routes
--
-- -> after that start with another singleton that is "far away" and start absorbing again.
-- how to pick the "other"

-- we might need to consider possible reorderings of routes
-- but if we "absorb" in the correct order, we should be able to avoid reorderings of routes