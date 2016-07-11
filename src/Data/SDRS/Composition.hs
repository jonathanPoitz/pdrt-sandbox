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
addDRS s@(SDRS m _) d edges = pureSDRS
  where updatedLast = (fst $ M.findMax m) + 1 -- before 1
        updatedOutscope = (fst $ M.findMax m) + 2 -- before 1, FIX this hack
        sdrsWithRelations = updateRelations s edges updatedLast updatedOutscope -- 1. step update relations (this must be before new segment to ensure right calculation of RF)
        sdrsWithSegment = addEDU sdrsWithRelations updatedLast (EDU d) -- 2. step - new segment
        sdrsWithNewLast = updateLast sdrsWithSegment updatedLast -- 3. step new last
        pureSDRS = sdrsAlphaConvertDRS sdrsWithNewLast updatedLast drsRefConvMap -- 4. drsAlphaConv
        accDRSs = accessibleDRSs sdrsWithNewLast updatedLast -- note: testwise, before it was sdrsWithRealtions -> -- note: this only works because the new drs is not in the sdrs yet, however the relation using its label is! this is of importance since in order to calculate the list of accessible drs, the new relation has to be there (as opposed to the new drs which first needs to be drsref-adjusted before being added)
        drsRefConvMap = buildDRSRefConvMap drsRefs1 drsRefs2
        drsRefs1 = concat $ map drsUniverse $ accDRSs
        drsRefs2 = drsUniverse d

-------------------------------------------------------------------------
-- | Adds 'SDRS' @s2@ and 'SDRS' @s1@. @s2@ is attached with its root node
-- to a node @dv1@ that must be on the RF of @s1@, using relation @r@.  
-------------------------------------------------------------------------
addSDRS :: SDRS -> SDRS -> [(DisVar,SDRSRelation)] -> SDRS
addSDRS s1@(SDRS m1 _) s2 edges = pureSDRS
  where convMap = buildConvMap s1 s2 -- before 1
        -- ^ a conversion map of all overlapping variables to new ones from both sdrss
        updatedLast = sdrsLast s2DVConv -- bw 1 and 2
        attachingNode = root s2DVConv -- bw 1 and 2
        updatedOutscope = (max (fst $ M.findMax m1) (fst $ M.findMax (sdrsMap s2DVConv))) + 1
        newDRSKeys = map fst $ segments s2DVConv -- bw 1 and 2
        accDRSs = accessibleDRSs sdrsMerged attachingNode -- bw 4 and 5
        drsRefs1 = concat $ map drsUniverse $ accDRSs -- bw 4 and 5
        -- ^ a list of all DRSRefs that is accessible from the attaching node after the update
        drsRefs2 = concat $ map drsUniverse $ drss s2DVConv -- bw 4 and 5
        -- ^ a list of all DRSRefs in the attaching SDRS
        drsRefConvMap = buildDRSRefConvMap drsRefs1 drsRefs2 -- bw 4 and 5
        -----
        s2DVConv = renameDisVars s2 convMap -- 1.
        s1WithNewRelation = updateRelations s1 edges attachingNode updatedOutscope -- 2.
        sdrsMerged = SDRS ((sdrsMap s1WithNewRelation) `M.union` (sdrsMap s2DVConv)) updatedLast -- 3. + 4.
        pureSDRS = sdrsAlphaConvertDRSs sdrsMerged newDRSKeys drsRefConvMap -- 5.
        
---------------------------------------------------------------------------
-- | adds one or more relations to the 'SDRS' @s@ between the newly added 'DisVar'
-- @newNode@ and one or several target nodes on the right frontier of @s@.
-- This involves making the necessary adjustments to the discourse structure
-- that depend on the place of attachment and the kind of attaching relation(s).
---------------------------------------------------------------------------
updateRelations :: SDRS -> [(DisVar, SDRSRelation)] -> DisVar -> DisVar -> SDRS
updateRelations _ [] _ _                    = error "Specify at least one relation!"
updateRelations s (t:[]) newNode newOutsc   = updateRelation s t newNode newOutsc
updateRelations s (t:rest) newNode newOutsc = updateRelations (updateRelation s t newNode newOutsc) rest newNode newOutsc

---------------------------------------------------------------------------
-- | Adds a relation to the 'SDRS' @s@ between the newly added 'DisVar'
-- @newNode@ and one target node on the right frontier of @s@.
-- This involves making the necessary adjustments to the discourse structure
-- that depend on the place of attachment and the kind of attaching relation.
---------------------------------------------------------------------------
updateRelation :: SDRS -> (DisVar,SDRSRelation) -> DisVar -> DisVar -> SDRS
updateRelation s (dv,rel) newNode newOutsc
  | isOnRF s dv && isRoot s dv             = trace (show "root") sdrsWithRel
  -- ^ the target node is the root node of the SDRS
  | isOnRF s dv && (not $ hasParents s dv) = trace (show "below root") sdrsWithNewConj
  -- ^ the target node is not the root node of the SDRS, but it is right underneath it (does not have incoming edges)
  | isOnRF s dv && isCrd rel &&
    isTopic rel                            = trace (show "not root, topic") sdrsWithRel
  -- ^ the target node is not the root node and the relation is a coordinating rel. that imposes a topic constraint
  | isOnRF s dv && isCrd rel               = trace (show "not root, normal crd") sdrsWithNewConj
  -- ^ the target node is not the root node and the relation is coordinating, but doesn't impose a topic constraint
  | isOnRF s dv                            = trace (show "not root, sub") sdrsWithNewConj
  -- ^ the target node is not the root node and the relation is subordinating
  | otherwise                              = trace (show "neither") s
  -- ^ skipping this relation because the target node is not on the RF of the SDRS
  where sdrsWithRightArgUpdate = trace "update" updateRightArgs s dv newOutsc -- 2. step - update all occurrences of dv as a right arg of a relation by replacing it with new outscoping label
        swapRels = trace "swapRels" calcLeftArgRels s dv
        sdrsWithRemovedSwapRels = trace "removeRels" removeRels sdrsWithRightArgUpdate swapRels -- 3. 
        sdrsWithSwapRels = trace "addswapRels" addCDUs sdrsWithRemovedSwapRels newOutsc swapRels -- 4. 
        sdrsWithRel = trace "addRel" addCDU sdrsWithSwapRels newOutsc (Relation rel dv newNode) -- 5. step - new relation
        sdrsWithNewConj = trace "addConj" addCDU s (lookupKey s dv) (Relation rel dv newNode) -- FIX order?

