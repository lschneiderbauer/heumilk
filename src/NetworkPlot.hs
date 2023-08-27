module NetworkPlot where

import Heumilk.Network
import Data.Graph.DGraph
import Data.Graph.Visualize
import Data.Graph.Types (Arc(..))
import Data.Bifunctor
import qualified GHC.Conc.Sync

pnToDGraph :: PN -> DGraph String Float
pnToDGraph pn = fromArcsList $ uncurry (uncurry Arc) <$> concat (zipWith comb (segToStr . segments <$> rs) (pallets <$> rs))
  where
    rs = routes pn
    comb segs pal = zip segs $ replicate (length segs) pal
    segToStr = map $ bimap show show
  
plotPN :: PN -> IO GHC.Conc.Sync.ThreadId
plotPN = plotDGraphEdged . pnToDGraph