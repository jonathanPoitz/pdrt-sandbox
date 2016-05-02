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
import Data.SDRS.DiscourseGraph
import Data.SDRS.Structure (lookupKey)
-- import Debug.Trace

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
-- TODO do RF checks
-- TODO what pre-checks need to be done?
-- -- check that node is part of sdrs
-- -- check that SDRS is well-formed?
---------------------------------------------------------------------------
--addDRS :: SDRS -> DRS -> [(DisVar, SDRSRelation)] -> SDRS
--addDRS s@(SDRS m _) d edges = SDRS updatedMap updatedLast
--  where updatedLast = (fst $ M.findMax m) + 1
--        updatedMap = M.insert updatedLast (Segment d) (updateRelations edges m updatedLast)
--        updateRelations :: [(DisVar, SDRSRelation)] -> M.Map DisVar SDRSFormula -> DisVar -> M.Map DisVar SDRSFormula
--        updateRelations [] m' _                      = m'
--        updateRelations ((dv, rel):rest) m' maxKey = if dv `elem` rf s
--                                                     then 
--                                                      if dv `elem` relLabels (M.elems m')
--                                                      then
--                                                       if relType rel == Crd
--                                                       then 
--                                                         if not $ isTopic rel
--                                                         then M.adjust (And (Relation rel dv updatedLast)) (lookupKey s dv) m'
--                                                         else 
--                                                           if -- relation pointing to target node is subordinating and entails \Downarrow (i.e., )
--                                                           then -- put new rel under new outscope and update old references to left arg with new outscoping label
--                                                           else -- not sure, we might have to make an explicit \Downarrow relation
--                                                       else M.adjust (And (Relation rel dv updatedLast)) (lookupKey s dv) m'
--                                                      else -- since we have done well-formedness checks, this can only be the root node of the sdrs.

--                                                        if relType rel == Crd
--                                                        then -- add it to the root node, put this relation under new outscoping label and update old references
--                                                        else -- also add to root node, put this relation under new outscoping label and update old references (but differently)
--                                                     else error = "Cannot add to node that's not on the RF"
--          --   relType rel == Crd  = M.map replaceByComplexNode mapWithNewNode -- iterate through map and find all relations where dv is second argument. replace this with M.findMax. todo: recurse somewhere
--          -- | otherwise           = M.insert (maxKey + 1) -- only do that if not crd! in what cases will case here happen even?
--          --                        (Relation rel dv updatedLast)
--          --                        (updateRelations rest m' (maxKey + 1)) 
--          where mapWithNewNode = M.insert (maxKey + 1) (Relation rel dv updatedLast) m'
--                replaceByComplexNode :: SDRSFormula -> SDRSFormula
--                replaceByComplexNode r@(Relation rel' dv' dv'')
--                  | dv'' == dv = Relation rel' dv' (maxKey + 1)
--                  | otherwise = r
--                replaceByComplexNode sf = sf

---------------------------------------------------------------------------
-- | adds a 'DRS' to an 'SDRS' given a number of edges, represented by
-- tuples consisting of the target node and the relation to that node.
-- TODO, needs to be debugged, e.g. addDRS sdrsfullal07_to4 _al07_5 [(2,relationFromLabel "Narration")] yields wrong result. (can't handle removal of relations from an and)
-- TODO disgustingly complicated, simplify!
---------------------------------------------------------------------------
addDRS :: SDRS -> DRS -> [(DisVar,SDRSRelation)] -> SDRS
addDRS s@(SDRS m _) d edges = SDRS updatedMap updatedLast
  where updatedLast = (fst $ M.findMax m) + 1 -- new reference to last 
        updatedMap = M.insert updatedLast (Segment d) (updateRelations edges m updatedLast) -- the new map with the added segment and the updated relations
        updateRelations :: [(DisVar, SDRSRelation)] -> M.Map DisVar SDRSFormula -> DisVar -> M.Map DisVar SDRSFormula
        updateRelations [] m' _                     = m'
        updateRelations ((dv, rel):rest) m' maxKey
          | isOnRF dv && isRoot dv                  = updateRelations rest (addNewRel m') (maxKey + 1) -- add it to the root node, put this relation under new outscoping label
          | isOnRF dv && isCrd rel &&
            isTopic rel && entailsTopic rel         = updateRelations rest (addNewRel updatedRefs) (maxKey + 1) -- put new rel under new outscope and update old references to left arg with new outscoping label
          | isOnRF dv && isCrd rel && 
            isTopic rel && (not $ entailsTopic rel) = updateRelations rest (addNewRel updatedRefs) (maxKey + 1) -- not sure, we might have to make an explicit \Downarrow relation
          | isOnRF dv && isCrd rel                  = updateRelations rest mapWithNewConj (maxKey + 1)
          | isOnRF dv                               = updateRelations rest mapWithNewConj (maxKey + 1)
          | otherwise                               = updateRelations rest m' maxKey -- skipping relation b/c target node is not on RF of SDRS
          where isOnRF :: DisVar -> Bool
                isOnRF dv' = dv' `elem` rf s
                isRoot :: DisVar -> Bool
                isRoot dv' = dv' == root s !! 0
                isCrd :: SDRSRelation -> Bool
                isCrd rel' = relType rel' == Crd
                entailsTopic :: SDRSRelation -> Bool
                entailsTopic _ = True -- TODO implement, which subRels entail \Downarrow?
                updatedRefs = updateRefs dv (maxKey + 1) m'
                addNewRel :: M.Map DisVar SDRSFormula -> M.Map DisVar SDRSFormula
                addNewRel m'' = M.insert (maxKey + 1) (Relation rel dv updatedLast) m''
                mapWithNewConj = M.adjust (And (Relation rel dv updatedLast)) (lookupKey s dv) m' -- add existing SDRSFormula with And to the new one. TODO, order? The old thing should be the first arg of And, right?
                updateRefs :: DisVar -> DisVar -> M.Map DisVar SDRSFormula -> M.Map DisVar SDRSFormula
                updateRefs old new m'' = M.map updateRef m''
                  where updateRef :: SDRSFormula -> SDRSFormula
                        updateRef seg@(Segment {})      = seg
                        updateRef r@(Relation rel' dv1 dv2)
                          | dv1 == old && dv2 == old    = Relation rel' new new -- shouldn't happen
                          | dv1 == old                  = Relation rel' new dv2
                          | dv2 == old                  = Relation rel' dv1 new -- this should then be replaced by the new outscoping label and put underneath that. how the hell will i do that?
                          | otherwise                   = r
                        updateRef (And sf1 sf2)         = And (updateRef sf1) (updateRef sf2)
                        updateRef (Not sf1)             = Not (updateRef sf1)

                      















