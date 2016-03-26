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
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.SDRS.Structure (expandRecursiveFormula)

---------------------------------------------------------------------------
-- | Checks if all labels are declared in an SDRS, i.e. that LAST is part 
-- of the keys of the map and that the arguments of all relations are as
-- well
---------------------------------------------------------------------------
noUndeclaredVars :: SDRS -> Bool
noUndeclaredVars (SDRS m l) = 
  (l `elem` disVars) && -- LAST is in the list of discourse variables
  relVars `Set.isProperSubsetOf` disVars -- the discourse variables used as arguments in relations are declared labels
    where relVars = Set.fromList $ relLabels (Map.elems m)
          disVars = Set.fromList $ Map.keys m

relLabels :: [SDRSFormula] -> [DisVar]
relLabels []                           = []
relLabels ((Relation _ dv1 dv2):rest)  = dv1:dv2:relLabels rest 
relLabels ((And sf1 sf2):rest)         = relLabels [sf1] ++ relLabels [sf2] ++ relLabels rest
relLabels ((Not sf1):rest)             = relLabels [sf1] ++ relLabels rest
relLabels ((Segment _):rest)              = relLabels rest

---------------------------------------------------------------------------
-- | Returns whether a given SDRS has any self referencing relations
---------------------------------------------------------------------------
noSelfRefs :: SDRS -> Bool
noSelfRefs (SDRS m _) = all noSelfRef (Map.assocs m)
  where noSelfRef :: (DisVar, SDRSFormula) -> Bool
        noSelfRef (dv0, Relation _ dv1 dv2)    = dv1 /= dv2 && dv0 /= dv1 && dv0 /= dv2
        noSelfRef (dv0, And sf1 sf2)           = noSelfRef (dv0,sf1) && noSelfRef (dv0,sf2)
        noSelfRef (dv0, Not sf1)               = noSelfRef (dv0,sf1)
        noSelfRef _ = True  

---------------------------------------------------------------------------
-- | Checks whether all Segments (directly labeled and labeled in recursive
-- SDRSFormulas) are bound in a Relation
---------------------------------------------------------------------------
allSegmentsBound :: SDRS -> Bool
allSegmentsBound (SDRS m _) = allSegmentLabels `Set.isSubsetOf` relArgs
  where allSegmentLabels = segmentLabels (Map.assocs m)
        relArgs = Set.fromList $ relLabels (Map.elems m)
        -- TODO there is probably some Data.Map function the below?
        segmentLabels :: [(DisVar, SDRSFormula)] -> (Set.Set DisVar)
        segmentLabels [] = Set.empty
        segmentLabels ((dv, (Segment _)):rest) = Set.insert dv $ segmentLabels rest
        -- there must be an easier way than makeTuple
        -- the below steps are needed to take care of cases where segments are not labeled directly but are introduced within an And/Not constructor
        segmentLabels ((dv, sf@(And _ _)):rest) = (segmentLabels $ map (makeTuple dv) (expandRecursiveFormula sf)) `Set.union` (segmentLabels rest)
        segmentLabels ((dv, sf@(Not _)):rest) = (segmentLabels $ map (makeTuple dv) (expandRecursiveFormula sf)) `Set.union` (segmentLabels rest)
        segmentLabels ((_, Relation _ _ _):rest) = segmentLabels rest
        makeTuple :: DisVar -> SDRSFormula -> (DisVar, SDRSFormula)
        makeTuple dv sf = (dv, sf)


-- how to make an sdrs unvalid:
-- 2. l not in m.keys
-- 3. rel labels not in m.keys
-- 4. rel labels same to referencing label
-- 5. rel labels not accessible
-- 6. last points to sth but Segment
-- 7. relations between sth but Segment
