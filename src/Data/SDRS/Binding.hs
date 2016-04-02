{- |
Module      :  Data.SDRS.Binding
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS binding
-}

module Data.SDRS.Binding
(
  noUndeclaredVars
, noSelfRefs
, allSegmentsBound
) where

import Data.SDRS.DataType
import qualified Data.Map as M
import Data.Set hiding (map)
import Data.SDRS.Structure (relLabels)

---------------------------------------------------------------------------
-- | Checks if all labels are declared in an SDRS, i.e. that LAST is part 
-- of the keys of the map and that the arguments of all relations are as
-- well
---------------------------------------------------------------------------
noUndeclaredVars :: SDRS -> Bool
noUndeclaredVars (SDRS m l) = 
  (l `member` disVars) && -- LAST is in the list of discourse variables
  relVars `isProperSubsetOf` disVars -- the discourse variables used as arguments in relations are declared labels
    where relVars = fromList $ relLabels (M.elems m)
          disVars = fromList $ M.keys m

---------------------------------------------------------------------------
-- | Returns whether a given SDRS has any self referencing relations
---------------------------------------------------------------------------
noSelfRefs :: SDRS -> Bool
noSelfRefs (SDRS m _) = all noSelfRef (M.assocs m)
  where noSelfRef :: (DisVar, SDRSFormula) -> Bool
        noSelfRef (dv0, Relation _ dv1 dv2) = dv1 /= dv2 && dv0 /= dv1 && dv0 /= dv2
        noSelfRef (dv0, And sf1 sf2)        = noSelfRef (dv0,sf1) && noSelfRef (dv0,sf2)
        noSelfRef (dv0, Not sf1)            = noSelfRef (dv0,sf1)
        noSelfRef _                         = True  

---------------------------------------------------------------------------
-- | Checks whether all Segments (directly labeled and labeled in recursive
-- SDRSFormulas) are bound in a Relation.
-- FIX: This doesn't produce right results when DRSs are introduced weirdly
-- within recursive SDRSFormulae
---------------------------------------------------------------------------
allSegmentsBound :: SDRS -> Bool
allSegmentsBound (SDRS m _) = allSegmentLabels `isSubsetOf` relArgs
  where allSegmentLabels = segmentLabels (M.assocs m)
        relArgs = fromList $ relLabels (M.elems m)
        segmentLabels :: [(DisVar, SDRSFormula)] -> Set DisVar
        segmentLabels []                       = empty
        segmentLabels ((dv, Segment _):rest)   = insert dv $ segmentLabels rest
        segmentLabels ((dv, And sf1 sf2):rest) = segmentLabels [(dv, sf1)] `union` segmentLabels [(dv, sf2)] `union` segmentLabels rest
        segmentLabels ((dv, Not sf1):rest)     =  segmentLabels [(dv, sf1)] `union` segmentLabels rest
        segmentLabels ((_, Relation {}):rest)  = segmentLabels rest

-- how to make an sdrs unvalid:
-- 2. l not in m.keys
-- 3. rel labels not in m.keys
-- 4. rel labels same to referencing label
-- 5. rel labels not accessible
-- 6. last points to sth but Segment
-- 7. relations between sth but Segment
