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
  allRelArgsBound
, noSelfRefs
, allSegmentsBound
) where

import Data.SDRS.DataType
import qualified Data.Map as M
import Data.Set hiding (map)
import Data.SDRS.Structure (relArgs)

---------------------------------------------------------------------------
-- | Checks whether any relation in the 'SDRS' @s@ uses labels as arguments
-- that are not a 'Segment' in @s@.
---------------------------------------------------------------------------
allRelArgsBound :: SDRS -> Bool
allRelArgsBound s@(SDRS m _) = relVars `isProperSubsetOf` disVars
  where relVars = fromList $ relArgs s
        disVars = fromList $ M.keys m

---------------------------------------------------------------------------
-- | Returns 'True' if the given 'SDRS' has no self-referencing relations
-- such as 3:Narration(4,4) or 3:Narration(3,4), otherwise returns 'False'.
---------------------------------------------------------------------------
noSelfRefs :: SDRS -> Bool
noSelfRefs (SDRS m _) = all noSelfRef (M.assocs m)
  where noSelfRef :: (DisVar, SDRSFormula) -> Bool
        noSelfRef (dv0, Relation _ dv1 dv2) = dv1 /= dv2 && dv0 /= dv1 && dv0 /= dv2
        noSelfRef (dv0, And sf1 sf2)        = noSelfRef (dv0,sf1) && noSelfRef (dv0,sf2)
        noSelfRef (dv0, Not sf1)            = noSelfRef (dv0,sf1)
        noSelfRef _                         = True  

---------------------------------------------------------------------------
-- | Checks for the given 'SDRS' @s@ whether all Segments are bound as an 
-- argument in a relation.
-- FIX: This doesn't produce right results when DRSs are introduced weirdly
-- within recursive SDRSFormulae
---------------------------------------------------------------------------
allSegmentsBound :: SDRS -> Bool
allSegmentsBound s@(SDRS m _) = allSegmentLabels `isSubsetOf` relArgs
  where allSegmentLabels = segmentLabels (M.assocs m)
        relArgs = fromList $ relArgs s
        segmentLabels :: [(DisVar, SDRSFormula)] -> Set DisVar
        segmentLabels []                       = empty
        segmentLabels ((dv, Segment _):rest)   = insert dv $ segmentLabels rest
        segmentLabels ((dv, And sf1 sf2):rest) = segmentLabels [(dv, sf1)] `union` segmentLabels [(dv, sf2)] `union` segmentLabels rest
        segmentLabels ((dv, Not sf1):rest)     = segmentLabels [(dv, sf1)] `union` segmentLabels rest
        segmentLabels ((_, Relation {}):rest)  = segmentLabels rest
