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
  addDRS
, addSDRS
, drsToSDRS 
) where

import qualified Data.Map as M

import Data.DRS.Structure

import Data.SDRS.DataType
import Data.SDRS.DiscourseStructure
import Data.SDRS.Structure
import Data.SDRS.LambdaCalculus
import Data.SDRS.Relation

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Adds 'DRS' @d@ to 'SDRS' @s@ using a list of edges, represented by
-- pairs consisting of the target node and the relation to that node. The
-- target nodes must lie on the right frontier of the @s1@, otherwise the
-- relation is skipped.
---------------------------------------------------------------------------
addDRS :: SDRS -> DRS -> [(DisVar,SDRSRelation)] -> SDRS
addDRS s@(SDRS m _) d edges = pureSDRS
  where updatedLast = (fst $ M.findMax m) + 1 -- before 1
        updatedOutscope = (fst $ M.findMax m) + 2 -- before 1, FIX this hack
        sdrsWithRelations = updateRelations s edges updatedLast updatedOutscope -- 1. step update relations (this must be before new segment to ensure right calculation of RF)
        sdrsWithSegment = addEDU sdrsWithRelations updatedLast (EDU d) -- 2. step - new segment
        sdrsWithNewLast = updateLast sdrsWithSegment updatedLast -- 3. step new last
        pureSDRS = sdrsAlphaConvertDRS sdrsWithNewLast updatedLast drsRefConvMap -- 4. drsAlphaConv
        accDRSs = accessibleDRSs sdrsWithNewLast updatedLast
        drsRefConvMap = buildDRSRefConvMap drsRefs1 drsRefs2
        drsRefs1 = concat $ map drsUniverse $ accDRSs
        drsRefs2 = drsUniverse d

-------------------------------------------------------------------------
-- | Adds 'SDRS' @s2@ to 'SDRS' @s1@ using a list of edges, represented by
-- pairs consisting of the target node and the relation to that node. The
-- target nodes must lie on the right frontier of the @s1@, otherwise the
-- relation is skipped.
-------------------------------------------------------------------------
addSDRS :: SDRS -> SDRS -> [(DisVar,SDRSRelation)] -> SDRS
addSDRS s1@(SDRS m1 _) s2 edges = pureSDRS
  where convMap = buildDisVarConvMap s1 s2 -- before 1
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
-- | Builds a new 'SDRS' from a 'DRS'.
---------------------------------------------------------------------------
drsToSDRS :: DRS -> SDRS
drsToSDRS d = SDRS (M.fromList [(0, EDU d)]) 0
        
---------------------------------------------------------------------------
-- * Private
---------------------------------------------------------------------------

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
  | isOnRF s dv && isRoot s dv             = sdrsWithRel -- the target node is the root node of the SDRS
  | isOnRF s dv && (not $ hasParents s dv) = sdrsWithNewConj -- the target node is not the root node of the SDRS, but it is right underneath it (does not have incoming edges)
  | isOnRF s dv && isTopic rel             = sdrsWithRel -- the target node is not the root node and the relation is a coordinating rel. that imposes a topic constraint
  | isOnRF s dv && isCrd rel               = sdrsWithNewConj -- the target node is not the root node and the relation is coordinating, but doesn't impose a topic constraint
  | isOnRF s dv                            = sdrsWithNewConj -- the target node is not the root node and the relation is subordinating
  | otherwise                              = s -- skipping this relation because the target node is not on the RF of the SDRS
  where sdrsWithRightArgUpdate = updateRightArgs s dv newOutsc -- 2. step - update all occurrences of dv as a right arg of a relation by replacing it with new outscoping label
        swapRels = calcLeftArgRels s dv
        sdrsWithRemovedSwapRels = removeRels sdrsWithRightArgUpdate swapRels -- 3. 
        sdrsWithSwapRels = addCDUs sdrsWithRemovedSwapRels newOutsc swapRels -- 4. 
        sdrsWithRel = addCDU sdrsWithSwapRels newOutsc (Relation rel dv newNode) -- 5. step - new relation
        sdrsWithNewConj = addCDU s (lookupKey s dv) (Relation rel dv newNode)

