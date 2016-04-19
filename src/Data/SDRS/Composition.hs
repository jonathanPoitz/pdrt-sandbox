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
buildFromDRSs :: SDRSRelation -> DRS -> DRS -> SDRS
buildFromDRSs rel d1 d2 = SDRS (M.fromList [(0, Relation rel 1 2),
                                              (1, Segment d1),
                                              (2, Segment d2)]) 2

---------------------------------------------------------------------------
-- | Adds a new 'DRS' @d@ to a given 'SDRS' @s@, using the given set of edges.
-- If the argument that is to be attached to in the set of edges is already part
-- of the SDRS, conjunct the newly created relation to the relations at this level.
---------------------------------------------------------------------------
addDRS :: SDRS -> DRS -> [(DisVar, SDRSRelation)] -> SDRS
addDRS s@(SDRS m _) d edges = SDRS updatedMap updatedLast
  where updatedLast = (fst $ M.findMax m) + 1
        updatedMap = M.insert updatedLast (Segment d) (updateRelations edges m updatedLast)
        updateRelations :: [(DisVar, SDRSRelation)] -> M.Map DisVar SDRSFormula -> DisVar -> M.Map DisVar SDRSFormula
        updateRelations [] m' _                      = m'
        updateRelations ((dv, rel):rest) m' maxKey 
          | dv `elem` relLabels (M.elems m') && (not $ isTopic rel) = M.adjust (And (Relation rel dv updatedLast)) (lookupKey s dv) m'
          | relType rel == Crd = M.map replaceByComplexNode mapWithNewNode -- iterate through map and find all relations where dv is second argument. replace this with M.findMax. todo: recurse somewhere
          | otherwise = M.insert (maxKey + 1) -- only do that if not crd! in what cases will case here happen even?
                                 (Relation rel dv updatedLast)
                                 (updateRelations rest m' (maxKey + 1)) 
          where mapWithNewNode = M.insert (maxKey + 1) (Relation rel dv updatedLast) m'
                replaceByComplexNode :: SDRSFormula -> SDRSFormula
                replaceByComplexNode r@(Relation rel' dv' dv'')
                  | dv'' == dv = Relation rel' dv' (maxKey + 1)
                  | otherwise = r
                replaceByComplexNode sf = sf



