{-# LANGUAGE FlexibleInstances #-}
module Heumilk.Network where

-- import Debug.Trace

import qualified Data.List as L
import qualified Data.Sequence as S
import Data.Sequence ( Seq (..), (><), (!?) )
import Data.List.Extra ( maximumOn )
import Data.List.Utils ( hasAny )
import qualified Data.Matrix.Unboxed as M
import qualified Data.Vector as V
import qualified Data.Bifunctor
import qualified Data.Foldable (toList)

type NPallets = Float
truckCapacity :: NPallets
truckCapacity = 33

-- sites are only stores
data Site = Origin | Site Int deriving (Eq)

instance Show Site where
  show Origin = "O"
  show (Site i) = "S" ++ show i

siteIndex :: Site -> Int
siteIndex Origin = 0
siteIndex (Site n) = n

type TruckID = Int
type TransportSegment = (Site, Site)

class (Eq a, Show a) => Route a where
  sites :: a -> Seq Site
  volume :: a -> NPallets
  appendSite :: a -> Site -> a
  dest :: a -> Site
  {-
  isCoveredBy :: a -> a -> Bool
  isCoveredBy tc1 tc2 = sites tc1 `L.isSubsequenceOf` sites tc2
  -}
  crossesSite :: a -> Site -> Bool
  crossesRoute :: a -> a -> Bool
  isSingleton :: a -> Bool
  segments :: a -> Seq TransportSegment
  -- prune :: a -> (Maybe a, TransportSegment) -- the dropped transport segment
  (=|=) :: a -> a -> Bool
  pp :: a -> String
  pp r = "{" ++ L.intercalate " -> " (show <$> Data.Foldable.toList (S.reverse $ sites r)) ++ "}"

-- by definiton TruckCycles start and end at DC
data TruckCycle = MkTruckCycle { tcSites :: Seq Site, initialLoad :: NPallets }
  deriving Eq

instance Show TruckCycle where
  show tc = "T<" ++ (show . initialLoad) tc ++ "> :: " ++ pp tc

instance Route TruckCycle where
  sites tc = (Origin :<| tcSites tc) :|> Origin
  appendSite tc s = tc { tcSites = s :<| tcSites tc }
  dest _ = Origin -- dummy for now
  crossesSite tc site = site `elem` S.drop 1 (tcSites tc)
  --crossesRoute tc1 tc2 = not $ disjoint (tcSites tc1) (tcSites tc2)
  crossesRoute tc1 tc2 = any (`elem` tcSites tc1) (tcSites tc2)
  segments tc = segsFromSeqs (sites tc)
  isSingleton tc = length (tcSites tc) == 1
  volume = initialLoad
  (=|=) = (==)
  {-
  prune :: TruckCycle -> (Maybe TruckCycle, TransportSegment)
  prune (MkPalletRoute (s1:<|s2:<|ss) pal) = (Just (MkPalletRoute (s2:<|ss) pal), (s1, s2))
  prune (MkPalletRoute (s1:<|Empty) pal) = (Nothing, (s1, Origin))
  
  prune tc = (maybeTc tc, head $ segments tc)
    where
      maybeTc tc
        | length (tcSites tc) <= 1 = Nothing  -- a PalletRoute with no sites should be Nothing
        | otherwise      = Just tc {tcSites = tail $ tcSites tc }
  -}

-- by definition PalletRoute starts at DC.
data PalletRoute = MkPalletRoute {prSites :: Seq Site, pallets :: NPallets}

instance Eq PalletRoute where
  (==) pr1 pr2 = prSites pr1 == prSites pr2

instance Show PalletRoute where
  show pr = "P<" ++ (show . pallets) pr ++ "> :: " ++ pp pr

instance Route PalletRoute where
  sites (MkPalletRoute ss _) = ss :|> Origin
  appendSite (MkPalletRoute ss n) s = MkPalletRoute (s :<| ss) n
  dest pr =
    case prSites pr of
      s1 :<| ss -> s1
      _ -> error "empty pallet route should not exist"
  crossesSite pr site = site `elem` S.drop 1 (prSites pr)
  --crossesRoute pr1 pr2 = not $ disjoint (prSites pr1) (prSites pr2)
  crossesRoute pr1 pr2 = any (`elem` prSites pr1) (prSites pr2)
  isSingleton pr = length (prSites pr) == 1
  segments pr = segsFromSeqs (sites pr)
  (=|=) pr1 pr2 = prSites pr1 == prSites pr2
  volume = pallets

prune :: PalletRoute -> (Maybe PalletRoute, TransportSegment)
prune (MkPalletRoute (s1:<|s2:<|ss) pal) = (Just (MkPalletRoute (s2:<|ss) pal), (s1, s2))
prune (MkPalletRoute (s1:<|Empty) pal) = (Nothing, (s1, Origin))
{-
prune pr = (maybePr pr, head $ segments pr)
  where
    maybePr (MkPalletRoute ss n)
      | length ss <= 1 = Nothing  -- a PalletRoute with no sites should be Nothing
      | otherwise      = Just (MkPalletRoute (tail ss) n)
-}

instance Semigroup PalletRoute where -- not commutative!
  pr1 <> pr2 = MkPalletRoute (prSites pr2 >< prSites pr1) (pallets pr2)


segsFromList :: Eq a => [a] -> [(a, a)]
segsFromList = zip <*> tail

segsFromSeqs :: Eq a => Seq a -> Seq (a, a)
segsFromSeqs = S.zip <*> shift
  where
    shift (s :<| tail) = tail

type DistanceMatrix = M.Matrix Float
distance :: DistanceMatrix -> Site -> Site -> Float
distance mat Origin Origin = mat M.! (0,0)
distance mat Origin (Site n) = mat M.! (0, n)
distance mat (Site n) (Site m) = mat M.! (n, m)
distance mat (Site n) Origin = mat M.! (0, n)


data Route a => Network a = MkNetwork { routes :: [a]
                                        , distances :: DistanceMatrix }

totalVolume :: Route a => Network a -> NPallets
totalVolume net = sum $ volume <$> routes net


instance Route a => Show (Network a) where
  show net = unlines (show <$> routes net)

instance Route a => Eq (Network a) where
  (==) net1 net2 = setequal (routes net1) (routes net2) && distances net1 == distances net2

setequal :: Eq a => [a] -> [a] -> Bool
setequal xs ys = null (xs L.\\ ys) && null (ys L.\\ xs)


-- replace every route that equals a given route 
-- if we pass Nothing, the route is removed from the Network
replacePRoute :: PN ->                -- network in which to make the replacement
                 PalletRoute ->       -- route to be replaced
                 Maybe PalletRoute -> -- replacement route
                 PN                   -- resulting network
replacePRoute net old_route Nothing = net { routes = L.delete old_route (routes net ) }
replacePRoute net old_route (Just new_route) = net {routes = replace (routes net) }
  where
    replace = (:) new_route . L.delete old_route

--  Leaf Routes are defined by having a destination
--  that is not within another route in that network
leafRoutes :: Route a => Network a -> [a]
leafRoutes net = filter isLeafRoute $ routes net where
  isLeafRoute r = not $ any (`crossesSite` dest r) (routes net)


leafRouteWithSite :: Route a => Network a -> Site -> Maybe a
leafRouteWithSite net site =
  case filter (`isEnd` site) (leafRoutes net) of
    [] -> Nothing
    (s:ss) -> Just s

  where
    isEnd pr site =
      case sites pr of
        start :<| tail -> site == start
        _ -> False


{-
coveredRoutes :: Route a => Network a -> a -> [a]
coveredRoutes net route = filter (`isCoveredBy` route) (routes net)
-}

type TN = Network TruckCycle
type PN = Network PalletRoute

pruneLeafPRoute ::  Network PalletRoute ->  ( Network PalletRoute  -- pruned network
                                            , PalletRoute          -- route that has been pruned
                                            , TransportSegment )   -- segment that needed to be removed
pruneLeafPRoute net = (replacePRoute net lr maybePrunedRoute, lr, seg)
  where
    -- lr = head $ leafRoutes net
    -- we might just assume that the longest route is a leafroute?
    lr = maximumOn (length . prSites) (routes net)
    (maybePrunedRoute, seg) = prune lr




{-
-- for a given segment check if there are additional trucks needed to cover
-- the pallet transportation
neededTruckloads :: PN -> TN -> TransportSegment -> [NPallets]
neededTruckloads pn tn seg = (pallets `natFit` truckCapacity) `natSub` n_trucks
  where
    pallets = palletsOnSegment seg pn
    existing_truckload = sum $ truckloadsOnSegment seg tn

-- todo: ? check if we have all sites in distances?
addNewTruckCycle :: [Site] -> NPallets -> TN -> TN
addNewTruckCycle sites load tn = tn { routes = MkTruckCycle sites load : routes tn }

addNewTruckCycles :: TN -> [Site] -> [NPallets] -> TN
addNewTruckCycles tn _ NZero = tn
addNewTruckCycles tn sites n = iterate (addNewTruckCycle sites) tn ! n
-}


{-
isPartOfRoute :: Route a => TransportSegment -> a -> Bool
isPartOfRoute (s1, s2) route =
  (s1 :<| s2 :<| Empty) `elem` S.chunksOf 2 (sites route)
-}

isPartOfRoute :: Route a => TransportSegment -> a -> Bool
isPartOfRoute (s1, s2) route =
  let
    ss = sites route
  in
    case S.elemIndexL s1 ss of
      Nothing -> False
      Just i ->
        case ss !? (i+1) of
          Nothing -> False
          Just k -> k == s2


requiredPallets :: PN -> TransportSegment -> NPallets
requiredPallets pn seg = sum $ pallets <$> filter (isPartOfRoute seg) (routes pn)

trucksOnSegment :: TN -> TransportSegment -> [TruckCycle]
trucksOnSegment tn seg = filter (isPartOfRoute seg) (routes tn)
{- 
truckloadsOnSegment :: TN -> TransportSegment -> [NPallets]
truckloadsOnSegment tn seg = initialLoad <$> trucksOnSegment tn seg
 -}

consumeTruckLoad :: TruckCycle -> NPallets -> (TruckCycle, NPallets)
--consumeTruckLoad tc new_load | traceShow (tc, new_load) False = undefined
consumeTruckLoad tc new_load = (tc { initialLoad = new_truckload }, rest)
  where
    new_truckload = min (initialLoad tc + new_load) truckCapacity
    rest = max (new_load - (new_truckload - initialLoad tc)) 0

dropTruckCycles :: TN -> [TruckCycle] -> TN
dropTruckCycles tn [] = tn
dropTruckCycles tn (tc:tcs) = dropped_tn { routes = L.delete tc (routes dropped_tn) }
  where
    dropped_tn = dropTruckCycles tn tcs

addTrucks :: TN -> Seq Site -> NPallets -> TN
-- addTrucks tn sites load | traceShow (tn, load) False = undefined
addTrucks tn sites load
  | load == 0 = tn
  | load <= truckCapacity = tn { routes = MkTruckCycle sites load : routes tn }
  | load > truckCapacity = addTrucks (addTrucks tn sites truckCapacity) sites (load - truckCapacity)

addRequiredTruckLoads :: TN -> Seq Site -> TransportSegment -> NPallets -> TN
addRequiredTruckLoads tn sites seg req_pallets
  | rest <= 0 = tn
  | otherwise = new_tn
  where
    existing_tcs = trucksOnSegment tn seg
    rest = req_pallets - sum (initialLoad <$> existing_tcs)

    -- initialLoad <$> existing_tcs <- this is a potential problem.
    -- we are relying on the fact that we do a recursive "cleanup" here which
    -- is something this function should not rely on. In general this calculation
    -- would not make sense I think.

    (mod_tcs, new_rest) = consume existing_tcs rest
      where
        consume :: [TruckCycle] -> NPallets -> ([TruckCycle], NPallets)
        -- consume tcs load | traceShow (tcs, load) False = undefined
        consume tcs 0 = (tcs, 0)
        consume [] load = ([], load)
        consume (tc:tcs) load = Data.Bifunctor.first (new_tcs :) x
          where
            (new_tcs, rest) = consumeTruckLoad tc load
            x = consume tcs rest

    new_tn_dropped = dropTruckCycles tn existing_tcs
    new_tn = addTrucks (new_tn_dropped { routes = routes new_tn_dropped ++ mod_tcs }) sites new_rest


routelessNet :: Route a => DistanceMatrix -> Network a
routelessNet = MkNetwork []

isRouteless :: Route a => Network a -> Bool
isRouteless = null . routes

-- return s subnetwork returning a Network only involving [Site] and the routes that cross these
-- routes
subNetwork :: Route a => Network a -> [Site] -> Network a
subNetwork net ss = net { routes = new_routes }
  where
    directly_involved_routes = filter (any (`elem` ss) . sites) (routes net)
    new_routes = filter (crossesAnyRoute directly_involved_routes) (routes net)
    crossesAnyRoute rs r = any (crossesRoute r) rs

tnFromPn :: PN -> TN
tnFromPn pn = snd $ until pred buildup (pn, routelessNet (distances pn) :: TN)
  where
    pred = isRouteless . fst

    buildup :: (PN, TN) -> (PN, TN)
    buildup (pn, tn)
      -- | traceShow (pn, tn) False = undefined
      | (not . isRouteless) pn  = (pruned_pn, new_tn)
      | otherwise               = (routelessNet (distances pn), tn)
      where
        (pruned_pn, effectedRoute, seg) = pruneLeafPRoute pn
        new_tn = addRequiredTruckLoads tn (prSites effectedRoute) seg (requiredPallets pn seg)

