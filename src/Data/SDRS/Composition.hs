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
  drsToSDRS 
, addDRS
, addSDRS
, removeRels
) where

import qualified Data.Map as M
import Debug.Trace
import Data.SDRS.Show()

import Data.SDRS.DataType
import Data.SDRS.DiscourseGraph
import Data.SDRS.Structure
import Data.SDRS.LambdaCalculus
import Data.SDRS.Relation

import Data.DRS.Structure

---------------------------------------------------------------------------
-- | Builds a new 'SDRS' from a single 'DRS'.
---------------------------------------------------------------------------
drsToSDRS :: DRS -> SDRS
drsToSDRS d = SDRS (M.fromList [(0, EDU d)]) 0

---------------------------------------------------------------------------
-- | adds a 'DRS' to an 'SDRS' given a number of edges, represented by
-- tuples consisting of the target node and the relation to that node.
-- TODO is it possible that more than one new outscope will be created (i.e.
-- Narration twice with different target nodes?)?
---------------------------------------------------------------------------
addDRS :: SDRS -> DRS -> [(DisVar,SDRSRelation)] -> SDRS
addDRS s@(SDRS m _) d edges = sdrsDRSRefAlphaConved
  where updatedLast = (fst $ M.findMax m) + 1 -- new reference to last 
        sdrsWithRelations = updateRelations s edges updatedLast updatedOutscope -- 1. step update relations (this must be before new segment to ensure right calculation of RF)
        sdrsWithSegment = addEDU sdrsWithRelations updatedLast (EDU d) -- 2. step - new segment
        sdrsWithNewLast = updateLast sdrsWithSegment updatedLast -- 3. step new last
        sdrsDRSRefAlphaConved = sdrsAlphaConvertDRS sdrsWithNewLast updatedLast drsRefConvMap
        accDRSs = accessibleDRSs sdrsWithRelations updatedLast -- note: this only works because the new drs is not in the sdrs yet, however the relation using its label is! this is of importance since in order to calculate the list of accessible drs, the new relation has to be there (as opposed to the new drs which first needs to be drsref-adjusted before being added)
        updatedOutscope = (fst $ M.findMax m) + 2 -- FIX this hack
        drsRefConvMap = buildDRSRefConvMap drsRefs1 drsRefs2
        drsRefs1 = concat $ map drsUniverse $ accDRSs
        drsRefs2 = drsUniverse d

-------------------------------------------------------------------------
-- | Adds 'SDRS' @s2@ and 'SDRS' @s1@. @s2@ is attached with its root node
-- to a node @dv1@ that must be on the RF of @s1@, using relation @r@.  
-------------------------------------------------------------------------
addSDRS :: SDRS -> SDRS -> [(DisVar,SDRSRelation)] -> SDRS
addSDRS s1@(SDRS m1 _) s2 edges = sdrsDRSRefAlphaConved
  where convMap = buildConvMap s1 s2 -- 1.
        drsRefs1 = concat $ map drsUniverse $ accDRSs -- 3a.
        -- ^ a list of all DRSRefs that is accessible from the attaching node after the update
        drsRefs2 = concat $ map drsUniverse $ drss s2DVConv -- 3b.
        -- ^ a list of all DRSRefs in the attaching SDRS
        drsRefConvMap = buildDRSRefConvMap drsRefs1 drsRefs2
        -- ^ a conversion map of all overlapping variables to new ones from both sdrss
        accDRSs = accessibleDRSs sdrsMerged attachingNode
        updatedLast = sdrsLast s2DVConv
        attachingNode = root s2DVConv
        updatedOutscope = (max (fst $ M.findMax m1) (fst $ M.findMax (sdrsMap s2DVConv))) + 1
        -----
        s2DVConv = renameDisVars s2 convMap -- 2.
        s1WithNewRelation = updateRelations s1 edges attachingNode updatedOutscope
        sdrsMerged = SDRS ((sdrsMap s1WithNewRelation) `M.union` (sdrsMap s2DVConv)) updatedLast -- merged maps and updated last
        newDRSKeys = map fst $ segments s2DVConv -- 2a. the labels of the drss that are to be added. these later need to be drsRefAlphaConv'ed
        sdrsDRSRefAlphaConved = sdrsAlphaConvertDRSs sdrsMerged newDRSKeys drsRefConvMap
        

---------------------------------------------------------------------------
-- | adds one or more relations to the 'SDRS' @s@ between the newly added 'DisVar'
-- @attachingNode@ and one or several target nodes on the right frontier of @s@.
-- This involves making the necessary adjustments to the discourse structure
-- that depend on the place of attachment and the kind of attaching relation.
---------------------------------------------------------------------------
updateRelations :: SDRS -> [(DisVar, SDRSRelation)] -> DisVar -> DisVar -> SDRS
updateRelations s [] _ _                   = s
updateRelations s ((dv, rel):rest) attachingNode updatedOutscope
  | isOnRF s dv && isRoot s dv             = trace (show "root") updateRelations sdrsWithRel rest attachingNode updatedOutscope
  -- ^ the target node is the root node of the SDRS
  | isOnRF s dv && (not $ hasParents s dv) = trace (show "below root") updateRelations sdrsWithNewConj rest attachingNode updatedOutscope
  -- ^ the target node is not the root node of the SDRS, but it is right underneath it (does not have incoming edges)
  | isOnRF s dv && isCrd rel &&
    isTopic rel                            = trace (show "not root, topic") updateRelations sdrsWithRel rest attachingNode updatedOutscope
  -- ^ the target node is not the root node and the relation is a coordinating rel. that imposes a topic constraint
  | isOnRF s dv && isCrd rel               = trace (show "not root, normal crd") updateRelations sdrsWithNewConj rest attachingNode updatedOutscope
  -- ^ the target node is not the root node and the relation is coordinating, but doesn't impose a topic constraint
  | isOnRF s dv                            = trace (show "not root, sub") updateRelations sdrsWithNewConj rest attachingNode updatedOutscope
  -- ^ the target node is not the root node and the relation is subordinating
  | otherwise                              = trace (show "neither") updateRelations s rest attachingNode updatedOutscope
  -- ^ skipping this relation because the target node is not on the RF of the SDRS
  where sdrsWithRightArgUpdate = updateRightArgs s dv updatedOutscope -- 2. step - update all occurrences of dv as a right arg of a relation by replacing it with new outscoping label
        swapRels = calcLeftArgRels s dv
        sdrsWithRemovedSwapRels = removeRels sdrsWithRightArgUpdate swapRels -- 3. 
        sdrsWithSwapRels = addCDUs sdrsWithRemovedSwapRels updatedOutscope swapRels -- 4. 
        sdrsWithRel = addCDU sdrsWithSwapRels updatedOutscope (Relation rel dv attachingNode) -- 5. step - new relation
        sdrsWithNewConj = addCDU s (lookupKey s dv) (Relation rel dv attachingNode) -- FIX order?
