{- |
Module      :  Data.SDRS.Composition
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS composition
-}

module Data.SDRS.Composition
(
  buildFromDRSs
, addDRS
) where

import Data.SDRS.DataType
import qualified Data.Map as M
import Data.SDRS.Structure (relLabels, lookupKey)

---------------------------------------------------------------------------
-- | Build new SDRS using two DRSs and their relation
---------------------------------------------------------------------------
buildFromDRSs :: DRS -> Label -> DRS -> SDRS
buildFromDRSs d1 label d2 = SDRS (M.fromList [(0, Relation label 1 2),
                                              (1, Segment d1),
                                              (2, Segment d2)]) 2

---------------------------------------------------------------------------
-- | Adds a new 'DRS' @d@ to a given 'SDRS' @s@, using the given set of edges.
-- If the argument that is to be attached to in the set of edges is already part
-- of the SDRS, conjunct the newly created relation to the relations at this level.
---------------------------------------------------------------------------
addDRS :: SDRS -> DRS -> [(DisVar, Label)] -> SDRS
addDRS s@(SDRS m _) d edges = SDRS (updatedMap) updatedLast
  where updatedLast = (fst $ M.findMax m) + 1
        updatedMap = M.insert updatedLast (Segment d) (updateRelations edges m updatedLast)
        conjunctRel :: SDRSFormula -> SDRSFormula -> SDRSFormula
        conjunctRel newSF oldSF = And oldSF newSF
        -- I don't think I have to distinguish between what is already there
        -- no matter what, I take the old SF and put an And around it and the new one
        --conjunctRel newSF oldSF@(Relation {}) = And oldSF newSF
        --conjunctRel newSF oldSF@(And {}) = And oldSF newSF
        updateRelations :: [(DisVar, Label)] -> M.Map DisVar SDRSFormula -> DisVar -> M.Map DisVar SDRSFormula
        updateRelations [] m' _                      = m'
        updateRelations ((dv, label):rest) m' maxKey 
          | dv `elem` relLabels (M.elems m') = M.adjust (conjunctRel (Relation label dv updatedLast)) (lookupKey s dv) m'
          | otherwise = M.insert (maxKey + 1)
                                 (Relation label dv updatedLast)
                                 (updateRelations rest m' (maxKey + 1))


