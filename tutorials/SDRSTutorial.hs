-- This is a tutorial for creating and manipulating SDRSs with PDRT-SANDBOX.

import qualified Data.Map as Map
import Data.SDRS

map1 = Map.fromList [(1,"a"),(2,"b"),(3,"c"),(4,"d")]


sdrs1 = SDRS (Map.fromList [(0, DRSForm $ DRS [] [])]) 0
