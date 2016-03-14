-- This is a tutorial for creating and manipulating SDRSs with PDRT-SANDBOX.

import qualified Data.Map as Map
import Data.SDRS

map1 = Map.fromList [(1,"a"),(2,"b"),(3,"c"),(4,"d")]

-- | "A man is happy."
drs1 = DRS [DRSRef "x"] 
                  [Rel (DRSRel "man") [DRSRef "x"]
                  ,Rel (DRSRel "happy") [DRSRef "x"]]

-- | "A woman is sad."
drs2 = DRS [DRSRef "x"] 
                  [Rel (DRSRel "woman") [DRSRef "x"]
                  ,Rel (DRSRel "sad") [DRSRef "x"]]

-- mergeDRS3 = Merge drs1 drs2
-- resolvedDRS3 = Data.DRS.drsResolveMerges $ mergeDRS3



sdrs1 = SDRS (Map.fromList [(0, DRSForm $ drs1),
														(1, DRSForm $ drs2),
														(2, RRel Parallel 0 1)]) 0

sdrs3 = SDRS (Map.fromList [(0, DRSForm $ DRS [] [])]) 0