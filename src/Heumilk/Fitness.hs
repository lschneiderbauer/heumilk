module Heumilk.Fitness where

import Data.List
import Data.List.Extra
import qualified Data.Matrix.Unboxed as M
import Heumilk.Nat
import Heumilk.Network

{-
data OrdSite a = OrdSite (Network a) Site deriving (Eq)

instance Route a => Show (OrdSite a) where
  show (OrdSite net s) = show s

instance Route a => Ord (OrdSite a) where
  compare (OrdSite net1 s1) (OrdSite net2 s2)
    | net1 == net2  = distance net1 Origin s1 `compare` distance net2 Origin s2
    | otherwise     = undefined -- Ord only makes sense within the same Network
-}

-- we are using here that Data.Set uses an ordering defined by Ord
netSites :: Route a => Network a -> [Site]
netSites net = foldr union [] (sites <$> routes net)

netSitesOrderedByDistance :: Route a => Network a -> Site -> [Site]
netSitesOrderedByDistance net s = sortOn (distance net s) (netSites net)

singletonRoutesOBDistance :: Route a => Network a -> Site -> [a]
singletonRoutesOBDistance net s = sortOn (distance net s . dest) $ filter isSingleton (routes net)

nearestSingletonLeafRoute :: Route a => Network a -> Site -> Maybe a
nearestSingletonLeafRoute net s
  | null filteredRoutes = Nothing
  | otherwise = Just $ minimumOn (distance net s . dest) filteredRoutes
  where
    filteredRoutes = filter pred (leafRoutes net)
    pred route = isSingleton route && dest route /= s

takeUntilEq :: Eq a => a -> [a] -> Maybe [a]
takeUntilEq until lst = elemIndex until lst >>= Just . (+ 1) >>= Just . (`take` lst)

attractRouteAtSite ::
  PalletRoute -> -- attractor route (the shorter one that pulls the other route onto)
  PalletRoute -> -- attracted route (the longer one, which will get modified)
  Site -> -- entry site of attracted route
  Maybe PalletRoute -- the modified route (if provided site does not match it returns nothing)
attractRouteAtSite pr_stable pr_mod entrySite = takeUntilEq entrySite (prSites pr_mod) >>= pr1_mod
  where
    pr1_mod subsites = Just pr_mod {prSites = subsites ++ prSites pr_stable}

attractSingletonRoute ::
  PalletRoute -> -- attractor route (the shorter one that pulls the other route onto)
  PalletRoute -> -- attracted route (the longer one, which will get modified)
  PalletRoute -- the modified route (if provided site does not match it returns nothing)
attractSingletonRoute pr_stable pr_mod = pr_mod {prSites = dest pr_mod : prSites pr_stable}

absorbNext :: PN -> PalletRoute -> Maybe (PN, PalletRoute)
absorbNext pn r1 = ret =<< nearestSingletonLeafRoute pn (dest r1)
  where
    ret r2 =
      let r2_mod = attractSingletonRoute r1 r2
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