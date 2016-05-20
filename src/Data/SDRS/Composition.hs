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

import qualified Data.Map as M
import Data.List (intersect)
import Debug.Trace
import Data.SDRS.Show()

import Data.SDRS.DataType
import Data.SDRS.DiscourseGraph
import Data.SDRS.Structure (lookupKey)
import Data.SDRS.LambdaCalculus (buildDRSRefConvMap)

-- import Data.DRS.Variables (drsVariables)
import Data.DRS.Structure (drsUniverse)
import Data.DRS.LambdaCalculus (renameSubDRS)
import Data.DRS.Merge ((<<+>>))

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
-- TODO is it possible that more than one new outscope will be created?
---------------------------------------------------------------------------
addDRS :: SDRS -> DRS -> [(DisVar,SDRSRelation)] -> SDRS
addDRS s@(SDRS m _) d edges = sdrsWithNewLast
  where updatedLast = (fst $ M.findMax m) + 1 -- new reference to last 
        sdrsWithRelations = updateRelations s edges updatedLast updatedOutscope -- 1. step update relations (this must be before new segment to ensure right calculation of RF)
        sdrsWithSegment = addRel sdrsWithRelations updatedLast (Segment alphaConvD) -- 2. step - new segment
        sdrsWithNewLast = updateLast sdrsWithSegment updatedLast -- 3. step new last
        alphaConvD = renameSubDRS d gd drsRefConvMap
        gd = foldl (<<+>>) (DRS [] []) accDRSs
        accDRSs = accessibleDRSs sdrsWithRelations updatedLast -- note: this only works because the new drs is not in the sdrs yet, however the relation using its label is! this is of importance since in order to calculate the list of accessible drs, the new relation has to be there (as opposed to the new drs which first needs to be drsref-adjusted before being added)
        updatedOutscope = (fst $ M.findMax m) + 2 -- FIX this is very hacky
        drsRefConvMap = buildDRSRefConvMap drsRefs1 drsRefOverlap
        drsRefs1 = concat $ map drsUniverse $ accDRSs -- FIXED, "drss s" takes all DRSs into account. we only want to calculate overlap with those that are actually accessible
        drsRefOverlap = drsRefs1 `intersect` (drsUniverse d) -- FIXED this should not be drsVariables here, cause then it looks in both the universe and the conds. but we only wanna replace those refs that have binding overlap (thus overlap in universes), right?

---------------------------------------------------------------------------
-- | adds one or more relations to the 'SDRS' @s@ between the newly added 'DisVar'
-- @attachingNode@ and one or several target nodes on the right frontier of @s@.
-- This involves making the necessary adjustments to the discourse structure
-- that depend on the place of attachment and the kind of attaching relation.
-- This function is exported b/c it's used by merge but it shouldn't be called
-- by the user. 
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
        sdrsWithSwapRels = addRels sdrsWithRemovedSwapRels updatedOutscope swapRels -- 4. 
        sdrsWithRel = addRel sdrsWithSwapRels updatedOutscope (Relation rel dv attachingNode) -- 5. step - new relation
        sdrsWithNewConj = addRel s (lookupKey s dv) (Relation rel dv attachingNode) -- FIX order?

---------------------------------------------------------------------------
-- | Private
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | updates the last pointer of an 'SDRS' @s@ to the new 'DisVar' @l@.
---------------------------------------------------------------------------
updateLast :: SDRS -> DisVar -> SDRS
updateLast (SDRS m _) l' = SDRS m l'

---------------------------------------------------------------------------
-- | Adds a given list of 'SDRSFormula'e as new conjuncts to an existing
-- 'SDRSFormula' that is labeled by the 'DisVar' @new@.
---------------------------------------------------------------------------
addRels :: SDRS -> DisVar -> [SDRSFormula] -> SDRS
addRels s _ []           = s
addRels s new (sf:rest) = addRels (addRel s new sf) new rest

---------------------------------------------------------------------------
-- | Given an 'SDRS' @s@, adds an SDRSFormula @sf@ as a new conjunct to an
-- existing 'SDRSFormula' that is labeled by the 'DisVar' @new@. If @new@
-- is not yet part of the 'SDRS', create it and have it label @sf@.
---------------------------------------------------------------------------
addRel :: SDRS -> DisVar -> SDRSFormula -> SDRS
addRel (SDRS m l) new sf 
  | M.member new m = SDRS (M.adjust (flip And sf) new m) l -- TODO switch args around?
  | otherwise      = SDRS (M.insert new sf m) l

---------------------------------------------------------------------------
-- | Calculates all relations within an 'SDRS' @s@ that have the 'DisVar' @old@
-- as their left argument.
---------------------------------------------------------------------------
calcLeftArgRels :: SDRS -> DisVar -> [SDRSFormula]
calcLeftArgRels (SDRS m _) old = reverse $ M.foldl putSwapRel [] m -- needs to be reversed in order to get right ordering in conjunction later
  where putSwapRel :: [SDRSFormula] -> SDRSFormula -> [SDRSFormula]
        putSwapRel acc (Segment {})  = acc
        putSwapRel acc r@(Relation _ dv1 _)
          | dv1 == old               = r:acc
          | otherwise                = acc
        putSwapRel acc (And sf1 sf2) = putSwapRel (putSwapRel acc sf1) sf2
        putSwapRel acc (Not sf1)     = putSwapRel acc sf1

---------------------------------------------------------------------------
-- | Within an 'SDRS' @s@ replaces all references of a given 'DisVar'
-- @old@ with @new@ iff @old@ occurs as a right argument of a relation.
---------------------------------------------------------------------------
updateRightArgs :: SDRS -> DisVar -> DisVar -> SDRS
updateRightArgs (SDRS m l) old new = SDRS (M.map updateR m) l
  where updateR :: SDRSFormula -> SDRSFormula
        updateR seg@(Segment {}) = seg
        updateR r@(Relation rel dv1 dv2)
          | dv2 == old           = Relation rel dv1 new
          | otherwise            = r
        updateR (And sf1 sf2)    = And (updateR sf1) (updateR sf2)
        updateR (Not sf1)        = Not (updateR sf1)

---------------------------------------------------------------------------
-- | removes all of the given 'SDRSFormula'e from the map of 'SDRSFormula'e
-- in an 'SDRS'.
---------------------------------------------------------------------------
removeRels :: SDRS -> [SDRSFormula] -> SDRS
removeRels s []        = s
removeRels s (sf:rest) = removeRels (removeRel s sf) rest

---------------------------------------------------------------------------
-- | removes a given 'SDRSFormula' @r@ from a given SDRS @s@ iff @r@ is a
-- Relation.
---------------------------------------------------------------------------
removeRel :: SDRS -> SDRSFormula -> SDRS
removeRel (SDRS m l) r@(Relation {}) = SDRS (M.map (removeRelFromSF r) m) l
removeRel s _                        = s

---------------------------------------------------------------------------
-- | checks whether 'DisVar' @dv@ in 'SDRS' @s@ is the root node.
---------------------------------------------------------------------------
isRoot :: SDRS -> DisVar -> Bool
isRoot s dv = dv == root s

---------------------------------------------------------------------------
-- | checks whether 'DisVar' @dv@ in 'SDRS' @s@ has any incoming edges.
---------------------------------------------------------------------------
hasParents :: SDRS -> DisVar -> Bool
hasParents (SDRS m _) dv = any incoming $ M.elems m
  where incoming :: SDRSFormula -> Bool
        incoming (Relation _ _ dv2) = dv == dv2
        incoming (And sf1 sf2)        = incoming sf1 || incoming sf2
        incoming (Not sf1)            = incoming sf1
        incoming (Segment _)          = False

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
removeRelFromSF :: SDRSFormula -> SDRSFormula -> SDRSFormula
removeRelFromSF r@(Relation {}) sf@(And {}) = recurse sf
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
removeRelFromSF _ sf = sf
