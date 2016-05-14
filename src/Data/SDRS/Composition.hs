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
, drsToSDRS 
, addDRS
, removeRel
, updateRelations
) where

import Data.SDRS.DataType
import Data.SDRS.DiscourseGraph
import Data.SDRS.Structure (lookupKey, drss)
import Data.SDRS.LambdaCalculus (buildDRSRefConvMap)

import qualified Data.Map as M
import Data.List (intersect)
--import Debug.Trace
--import Data.SDRS.Show()

import Data.DRS.Structure (drsUniverse)
import Data.DRS.LambdaCalculus (drsAlphaConvert)


---------------------------------------------------------------------------
-- | Builds a new 'SDRS' from a single 'DRS'.
---------------------------------------------------------------------------
drsToSDRS :: DRS -> SDRS
drsToSDRS d = SDRS (M.fromList [(0, Segment d)]) 0

---------------------------------------------------------------------------
-- | Builds a new 'SDRS' using two 'DRS's and their relations
-- TODO clumsy and redundant, remove?
---------------------------------------------------------------------------
buildFromDRSs :: [SDRSRelation] -> DRS -> DRS -> SDRS
buildFromDRSs rels d1 d2 = addDRS sdrs1 d2 edges
  where sdrs1 = drsToSDRS d1
        edges = zip (repeat $ root sdrs1) rels

---------------------------------------------------------------------------
-- | adds a 'DRS' to an 'SDRS' given a number of edges, represented by
-- tuples consisting of the target node and the relation to that node.
---------------------------------------------------------------------------
addDRS :: SDRS -> DRS -> [(DisVar,SDRSRelation)] -> SDRS
addDRS s@(SDRS m _) d edges = SDRS updatedMap updatedLast
  where updatedLast = (fst $ M.findMax m) + 1 -- new reference to last 
        updatedMap = updateRelations s edges mWithSegment updatedLast updatedOutscope -- the new map with the added segment and the updated relations
        mWithSegment = M.insert updatedLast (Segment alphaConvD) m -- 1. step - new segment
        alphaConvD = drsAlphaConvert d drsRefConvMap
        updatedOutscope = (fst $ M.findMax mWithSegment) + 1
        drsRefConvMap = buildDRSRefConvMap drsRefs1 drsRefOverlap
        drsRefs1 = concat $ map drsUniverse $ drss s
        drsRefOverlap = drsRefs1 `intersect` (drsUniverse d)

---------------------------------------------------------------------------
-- | adds one or more relations to the 'SDRS' @s@ between the newly added 'DisVar'
-- @attachingNode@ and one or several target nodes on the right frontier of @s@.
-- This involves making the necessary adjustments to the discourse structure
-- that depend on the place of attachment and the kind of attaching relation.
-- This function is exported b/c it's used by merge but it shouldn't be called
-- by the user. 
---------------------------------------------------------------------------
updateRelations :: SDRS -> [(DisVar, SDRSRelation)] -> M.Map DisVar SDRSFormula -> DisVar -> DisVar -> M.Map DisVar SDRSFormula
updateRelations _ [] mArg _ _               = mArg
updateRelations s ((dv, rel):rest) mArg attachingNode updatedOutscope
  | isOnRF s dv && isRoot s dv              = updateRelations s rest mWithSwapRels attachingNode updatedOutscope
  -- ^ the target node is the root node of the SDRS
  | isOnRF s dv && isCrd rel &&
    isTopic rel && entailsTopic rel         = updateRelations s rest mWithSwapRels attachingNode updatedOutscope
  -- ^ the target node is not the root node and the relation is a coordinating rel. that imposes a topic constraint
  | isOnRF s dv && isCrd rel && 
    isTopic rel && (not $ entailsTopic rel) = updateRelations s rest mWithSwapRels attachingNode updatedOutscope
  -- ^ the target node is not the root node and the relation is a coordinating rel. that imposes a topic constraint,
  -- not sure if something changes here, we might have to make an explicit \Downarrow relation
  | isOnRF s dv && isCrd rel                = updateRelations s rest mapWithNewConj attachingNode updatedOutscope
  -- ^ the target node is not the root node and the relation is coordinating, but doesn't impose a topic constraint
  | isOnRF s dv                             = updateRelations s rest mapWithNewConj attachingNode updatedOutscope
  -- ^ the target node is not the root node and the relation is subordinating
  | otherwise                               = updateRelations s rest mArg attachingNode updatedOutscope
  -- ^ skipping this relation because the target node is not on the RF of the SDRS
  where 
        entailsTopic :: SDRSRelation -> Bool
        entailsTopic _ = True -- TODO implement, which subRels entail \Downarrow?
        -----------
        mWithRel = M.insert updatedOutscope (Relation rel dv attachingNode) mArg -- 2. step - new relation
        mWithRightArgUpdate = updateRightArgs mWithRel dv updatedOutscope -- 3. step - update all occurrences of dv as a right arg of a relation by replacing it with new outscoping label
        swapRels = calcLeftArgRels mArg dv
        mWithRemovedSwapRels = removeRels swapRels mWithRightArgUpdate
        mWithSwapRels = addRels updatedOutscope swapRels mWithRemovedSwapRels
        mapWithNewConj = M.adjust (flip And (Relation rel dv attachingNode)) (lookupKey s dv) mArg -- add existing SDRSFormula with And to the new one.

---------------------------------------------------------------------------
-- | Private
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Adds a given list of 'SDRSFormula'e as new conjuncts to an existing
-- 'SDRSFormula' that is labeled by the 'DisVar' @new@.
---------------------------------------------------------------------------
addRels :: DisVar -> [SDRSFormula] -> M.Map DisVar SDRSFormula -> M.Map DisVar SDRSFormula
addRels _ [] mArg'                        = mArg'
addRels new (r@(Relation {}):rest') mArg' = addRels new rest' (M.adjust (And r) new mArg') -- TODO switch args around?
addRels new (_:rest') mArg'               = addRels new rest' mArg'

---------------------------------------------------------------------------
-- | Calculates all relations within an 'SDRS' that have the 'DisVar' @old@
-- as their left argument.
---------------------------------------------------------------------------
calcLeftArgRels :: M.Map DisVar SDRSFormula -> DisVar -> [SDRSFormula]
calcLeftArgRels mArg old = M.foldl putSwapRel [] mArg
  where putSwapRel :: [SDRSFormula] -> SDRSFormula -> [SDRSFormula]
        putSwapRel acc (Segment {})  = acc
        putSwapRel acc r@(Relation _ dv1 _)
          | dv1 == old               = r:acc
          | otherwise                = acc
        putSwapRel acc (And sf1 sf2) = putSwapRel (putSwapRel acc sf1) sf2
        putSwapRel acc (Not sf1)     = putSwapRel acc sf1

---------------------------------------------------------------------------
-- | Within an 'SDRS' @mArg@ replaces all references of a given 'DisVar'
-- @old@ with @new@ iff @old@ occurs as a right argument of a relation.
---------------------------------------------------------------------------
updateRightArgs :: M.Map DisVar SDRSFormula -> DisVar -> DisVar -> M.Map DisVar SDRSFormula
updateRightArgs mArg old new = M.map updateR mArg
  where updateR :: SDRSFormula -> SDRSFormula
        updateR seg@(Segment {}) = seg
        updateR r@(Relation rel' dv1 dv2)
          | dv2 == old           = Relation rel' dv1 new
          | otherwise            = r
        updateR (And sf1 sf2)    = And (updateR sf1) (updateR sf2)
        updateR (Not sf1)        = Not (updateR sf1)

---------------------------------------------------------------------------
-- | removes all of the given 'SDRSFormula'e from the map of 'SDRSFormula'e
-- in an 'SDRS'.
---------------------------------------------------------------------------
removeRels :: [SDRSFormula] -> M.Map DisVar SDRSFormula -> M.Map DisVar SDRSFormula
removeRels [] mArg                      = mArg
removeRels (r@(Relation {}):rest') mArg = removeRels rest' (removeRel' r mArg)
  where removeRel' :: SDRSFormula -> M.Map DisVar SDRSFormula -> M.Map DisVar SDRSFormula
        removeRel' r'@(Relation {}) mArg' = M.map (removeRel r') mArg'
        removeRel' _ mArg'                = mArg'
removeRels (_:rest') mArg               = removeRels rest' mArg

---------------------------------------------------------------------------
-- | checks whether 'DisVar' @dv@ in 'SDRS' @s@ is the root node.
---------------------------------------------------------------------------
isRoot :: SDRS -> DisVar -> Bool
isRoot s dv = dv == root s

---------------------------------------------------------------------------
-- | checks whether the given 'SDRSRelation' @rel@ is of relation type
-- Coordinating 
---------------------------------------------------------------------------
isCrd :: SDRSRelation -> Bool
isCrd rel = relType rel == Crd

---------------------------------------------------------------------------
-- | checks whether a given 'DisVar' @dv@ is on the right frontier of 
-- 'SDRS' @s@.
---------------------------------------------------------------------------
isOnRF :: SDRS -> DisVar -> Bool
isOnRF s dv = dv `elem` rf s

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
