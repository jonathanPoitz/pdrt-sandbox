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
	listUnboundMapKeys
, listUnboundDisVars
, checkNoUnboundVars
) where

import Data.SDRS.DataType
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ((\\))

---------------------------------------------------------------------------
-- | Returns a list of DisVar that is mapped to an SDRSFormula but not part
-- of the set of DisVars
---------------------------------------------------------------------------
listUnboundMapKeys :: SDRS -> [DisVar]
listUnboundMapKeys (SDRS a m _) = Map.keys m \\ a

---------------------------------------------------------------------------
-- | Returns a list of DisVar that is part of the set of DisVars but not 
-- mapped to an SDRSFormula
---------------------------------------------------------------------------
listUnboundDisVars :: SDRS -> [DisVar]
listUnboundDisVars (SDRS a m _) = a \\ Map.keys m

-----------------------------------------------------------------------------
---- | Checks if all labels are bound in an SDRS
-----------------------------------------------------------------------------
checkNoUnboundVars :: SDRS -> Bool
checkNoUnboundVars (SDRS a m l) = 
  l `elem` a &&
  (Map.keys m) == a &&
  Set.fromList (getAllLabels (Map.elems m)) == Set.fromList a &&
  checkNoSelfRefRels (Map.assocs m)
    where getAllLabels :: [SDRSFormula] -> [DisVar]
          getAllLabels []                           = []
          getAllLabels ((Relation _ dv1 dv2):rest)  = dv1:dv2:getAllLabels rest 
          getAllLabels ((And sf1 sf2):rest)         = getAllLabels [sf1] ++ getAllLabels [sf2] ++ getAllLabels rest
          getAllLabels ((Not sf1):rest)             = getAllLabels [sf1] ++ getAllLabels rest
          -- Text and Segment constructors are skipped since they don't have labels
          getAllLabels (_:rest)              = getAllLabels rest
          checkNoSelfRefRels :: ([(DisVar, SDRSFormula)]) -> Bool
          checkNoSelfRefRels list = all noSelfRef list
            where noSelfRef :: (DisVar, SDRSFormula) -> Bool
                  noSelfRef (dv0, Relation _ dv1 dv2)    = dv1 /= dv2 && dv0 /= dv1 && dv0 /= dv2
                  noSelfRef (dv0, And sf1 sf2)           = noSelfRef (dv0,sf1) && noSelfRef (dv0,sf2)
                  noSelfRef (dv0, Not sf1)               = noSelfRef (dv0,sf1)
                  noSelfRef _ = True


-- how to make an sdrs unvalid:
-- 1. set(a) /= set(m.keys)
-- 2. l not in a
-- 3. rel labels not in a
-- 4. rel labels same to referencing label
-- 5. rel labels not accessible
