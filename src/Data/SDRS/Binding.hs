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
) where

import Data.SDRS.DataType
import qualified Data.Map as Map
import qualified Data.Set as Set

---------------------------------------------------------------------------
-- | Checks if all labels are declared in an SDRS, i.e. that LAST is part 
-- of the keys of the map and that the arguments of all relations are as
-- well
---------------------------------------------------------------------------
noUndeclaredVars :: SDRS -> Bool
noUndeclaredVars (SDRS m l) = 
  (l `elem` disVars) && -- LAST is in the list of discourse variables
  relVars `Set.isProperSubsetOf` disVars -- the discourse variables used as arguments in relations are declared labels
    where getAllLabels :: [SDRSFormula] -> [DisVar]
          getAllLabels []                           = []
          getAllLabels ((Relation _ dv1 dv2):rest)  = dv1:dv2:getAllLabels rest 
          getAllLabels ((And sf1 sf2):rest)         = getAllLabels [sf1] ++ getAllLabels [sf2] ++ getAllLabels rest
          getAllLabels ((Not sf1):rest)             = getAllLabels [sf1] ++ getAllLabels rest
          getAllLabels ((Segment _):rest)              = getAllLabels rest
          relVars = Set.fromList $ getAllLabels (Map.elems m)
          disVars = Set.fromList $ Map.keys m

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


-- how to make an sdrs unvalid:
-- 2. l not in m.keys
-- 3. rel labels not in m.keys
-- 4. rel labels same to referencing label
-- 5. rel labels not accessible
-- 6. last points to sth but Segment
-- 7. relations between sth but Segment
