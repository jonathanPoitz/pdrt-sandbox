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
, removeRel
) where

import Data.SDRS.DataType
import qualified Data.Map as M
import Data.SDRS.DiscourseGraph
import Data.SDRS.Structure (lookupKey)
--import Debug.Trace
--import Data.SDRS.Show()

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
---------------------------------------------------------------------------
addDRS :: SDRS -> DRS -> [(DisVar,SDRSRelation)] -> SDRS
addDRS s@(SDRS m _) d edges = SDRS updatedMap updatedLast
  where updatedLast = (fst $ M.findMax m) + 1 -- new reference to last 
        updatedMap = updateRelations edges mWithSegment updatedLast -- the new map with the added segment and the updated relations
        mWithSegment = M.insert updatedLast (Segment d) m -- 1. step - new segment
        updateRelations :: [(DisVar, SDRSRelation)] -> M.Map DisVar SDRSFormula -> DisVar -> M.Map DisVar SDRSFormula
        updateRelations [] mArg _                     = mArg
        updateRelations ((dv, rel):rest) mArg maxKey
          | isOnRF dv && isRoot dv                  = updateRelations rest mWithSwapRels updatedOutscope
          | isOnRF dv && isCrd rel &&
            isTopic rel && entailsTopic rel         = updateRelations rest mWithSwapRels updatedOutscope
          | isOnRF dv && isCrd rel && 
            isTopic rel && (not $ entailsTopic rel) = updateRelations rest mWithSwapRels updatedOutscope -- not sure, we might have to make an explicit \Downarrow relation
          | isOnRF dv && isCrd rel                  = updateRelations rest mapWithNewConj (maxKey + 1)
          | isOnRF dv                               = updateRelations rest mapWithNewConj (maxKey + 1)
          | otherwise                               = updateRelations rest mArg maxKey -- skipping relation b/c target node is not on RF of SDRS
          where isOnRF :: DisVar -> Bool
                isOnRF dv' = dv' `elem` rf s
                isRoot :: DisVar -> Bool
                isRoot dv' = dv' == root s !! 0
                isCrd :: SDRSRelation -> Bool
                isCrd rel' = relType rel' == Crd
                entailsTopic :: SDRSRelation -> Bool
                entailsTopic _ = True -- TODO implement, which subRels entail \Downarrow?
                -----------
                updatedOutscope = maxKey + 1
                mWithRel = M.insert updatedOutscope (Relation rel dv updatedLast) mArg -- 2. step - new relation
                mWithRightArgUpdate = updateRightArgs mWithRel dv updatedOutscope -- 3. step - update all occurrences of dv as a right arg of a relation by replacing it with new outscoping label
                swapRels = calcSwapRels mArg dv
                mWithRemovedSwapRels = removeOldSwapRels updatedOutscope swapRels mWithRightArgUpdate
                mWithSwapRels = addSwapRelsToMap updatedOutscope swapRels mWithRemovedSwapRels
                mapWithNewConj = M.adjust (flip And (Relation rel dv updatedLast)) (lookupKey s dv) mArg -- add existing SDRSFormula with And to the new one.
                removeOldSwapRels :: DisVar -> [SDRSFormula] -> M.Map DisVar SDRSFormula -> M.Map DisVar SDRSFormula
                removeOldSwapRels _ [] mArg'                        = mArg'
                removeOldSwapRels new (r@(Relation {}):rest') mArg' = removeOldSwapRels new rest' (removeSwapRel r mArg')
                  where removeSwapRel :: SDRSFormula -> M.Map DisVar SDRSFormula -> M.Map DisVar SDRSFormula
                        removeSwapRel r'@(Relation {}) mArg'' = M.map (removeRel r') mArg'' -- remove everywhere but in new one
                        removeSwapRel _ mArg''                = mArg''
                removeOldSwapRels new (_:rest') mArg'               = removeOldSwapRels new rest' mArg'
                -----------
                addSwapRelsToMap :: DisVar -> [SDRSFormula] -> M.Map DisVar SDRSFormula -> M.Map DisVar SDRSFormula
                addSwapRelsToMap _ [] mArg'                        = mArg'
                addSwapRelsToMap new (r@(Relation {}):rest') mArg' = addSwapRelsToMap new rest' (M.adjust (And r) new mArg') -- TODO switch args around?
                addSwapRelsToMap new (_:rest') mArg'               = addSwapRelsToMap new rest' mArg'
                -----------
                calcSwapRels :: M.Map DisVar SDRSFormula -> DisVar -> [SDRSFormula]
                calcSwapRels mArg' old = M.foldl putSwapRel [] mArg'
                  where putSwapRel :: [SDRSFormula] -> SDRSFormula -> [SDRSFormula]
                        putSwapRel acc (Segment {})  = acc
                        putSwapRel acc r@(Relation _ dv1 _)
                          | dv1 == old               = r:acc
                          | otherwise                = acc
                        putSwapRel acc (And sf1 sf2) = putSwapRel (putSwapRel acc sf1) sf2
                        putSwapRel acc (Not sf1)     = putSwapRel acc sf1
                -----------
                updateRightArgs :: M.Map DisVar SDRSFormula -> DisVar -> DisVar -> M.Map DisVar SDRSFormula
                updateRightArgs mArg' old new = M.map updateR mArg'
                  where updateR :: SDRSFormula -> SDRSFormula
                        updateR seg@(Segment {}) = seg
                        updateR r@(Relation rel' dv1 dv2)
                          | dv2 == old           = Relation rel' dv1 new
                          | otherwise            = r
                        updateR (And sf1 sf2)    = And (updateR sf1) (updateR sf2)
                        updateR (Not sf1)        = Not (updateR sf1)
                      

---------------------------------------------------------------------------
-- | Private
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Given a conjunction of 'SDRSFormula'e @sf@, removes the subrelation @r@
-- from the conjunction.
---------------------------------------------------------------------------
removeRel :: SDRSFormula -> SDRSFormula -> SDRSFormula
removeRel r@(Relation {}) sf@(And {}) = recurse sf
  where recurse :: SDRSFormula -> SDRSFormula
        recurse (And sf1@(And {}) sf2@(And {})) = And (recurse sf1) (recurse sf2)
        recurse (And sf1@(And {}) sf2@(Relation {}))
          | r == sf2                            = recurse sf1
          | otherwise                           = And (recurse sf1) sf2
        recurse (And sf1@(Relation {}) sf2@(And {}))
          | r == sf1                            = recurse sf2
          | otherwise                           = And sf1 (recurse sf2)
        recurse a@(And sf1@(Relation {}) sf2@(Relation {}))
          | r == sf1                            = sf2
          | r == sf2                            = sf1
          | otherwise                           = a
        recurse sf'                             = sf'
removeRel _ sf = sf






