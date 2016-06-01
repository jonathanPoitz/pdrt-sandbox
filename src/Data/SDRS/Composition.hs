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
, removeRel
, updateRelations
) where

import qualified Data.Map as M
import Debug.Trace
import Data.SDRS.Show()

import Data.SDRS.DataType
import Data.SDRS.DiscourseGraph
import Data.SDRS.Structure (lookupKey)
import Data.SDRS.LambdaCalculus
import Data.SDRS.Relation

import Data.DRS.Structure (drsUniverse)

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
        updatedOutscope = (fst $ M.findMax m) + 2 -- FIX this is very hacky
        drsRefConvMap = buildDRSRefConvMap drsRefs1 drsRefs2
        drsRefs1 = concat $ map drsUniverse $ accDRSs
        drsRefs2 = drsUniverse d

---------------------------------------------------------------------------
-- | adds one or more relations to the 'SDRS' @s@ between the newly added 'DisVar'
-- @attachingNode@ and one or several target nodes on the right frontier of @s@.
-- This involves making the necessary adjustments to the discourse structure
-- that depend on the place of attachment and the kind of attaching relation.
-- This function is exported because it's used by merge but it shouldn't be called
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
        sdrsWithSwapRels = addCDUs sdrsWithRemovedSwapRels updatedOutscope swapRels -- 4. 
        sdrsWithRel = addCDU sdrsWithSwapRels updatedOutscope (Relation rel dv attachingNode) -- 5. step - new relation
        sdrsWithNewConj = addCDU s (lookupKey s dv) (Relation rel dv attachingNode) -- FIX order?

---------------------------------------------------------------------------
-- | Private
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | updates the last pointer of an 'SDRS' @s@ to the new 'DisVar' @l@.
---------------------------------------------------------------------------
updateLast :: SDRS -> DisVar -> SDRS
updateLast (SDRS m _) l' = SDRS m l'

---------------------------------------------------------------------------
-- | Conjuncts a given list of 'CDU's to a given 'DisVar' @new@ in an
-- 'SDRS' @s@. If @new@ does not yet exist, it is created first.
---------------------------------------------------------------------------
addCDUs :: SDRS -> DisVar -> [CDU] -> SDRS
addCDUs s _ []           = s
addCDUs s new (cdu:rest)  = addCDUs (addCDU s new cdu) new rest

---------------------------------------------------------------------------
-- | Given an 'SDRS' @s@, adds a CDU @cdu@ as a new conjunct to an
-- existing 'CDU' which is embedded in an 'SDRSFormula' that is labeled by
-- the 'DisVar' @new@. If @new@ is not yet part of the 'SDRS', create it
-- and have it label a new 'SDRSFormula' containing @cdu@.
---------------------------------------------------------------------------
addCDU :: SDRS -> DisVar -> CDU -> SDRS
addCDU (SDRS m l) new cdu 
  | M.member new m = SDRS (M.insert new newSF m) l
  | otherwise      = SDRS (M.insert new (CDU cdu) m) l
  where newSF = CDU $ And cdu $ extractCDU $ m M.! new

---------------------------------------------------------------------------
-- | Given an 'SDRS' @s@, a 'DisVar' @new@ and an 'SDRSFormula' @edu@, adds
-- @edu@ to the 'SDRS' labeled by @new@. If @new@ already is a label in the map,
-- replace its value by @edu@.
---------------------------------------------------------------------------
addEDU :: SDRS -> DisVar -> SDRSFormula -> SDRS
addEDU (SDRS m l) new edu = SDRS (M.insert new edu m) l

---------------------------------------------------------------------------
-- | Calculates all relations within an 'SDRS' @s@ that have the 'DisVar' @old@
-- as their left argument.
---------------------------------------------------------------------------
calcLeftArgRels :: SDRS -> DisVar -> [CDU]
calcLeftArgRels (SDRS m _) old = reverse $ M.foldl putSwapRel [] m -- needs to be reversed in order to get right ordering in conjunction later
  where putSwapRel :: [CDU] -> SDRSFormula -> [CDU]
        putSwapRel acc (EDU {})  = acc
        putSwapRel acc (CDU r@(Relation _ dv1 _))
          | dv1 == old               = r:acc
          | otherwise                = acc
        putSwapRel acc (CDU (And sf1 sf2)) = putSwapRel (putSwapRel acc (CDU sf1)) (CDU sf2)
        putSwapRel acc (CDU (Not sf1))     = putSwapRel acc (CDU sf1)

---------------------------------------------------------------------------
-- | removes all of the given 'CDU's from the map of 'SDRSFormula'e
-- in an 'SDRS'.
---------------------------------------------------------------------------
removeRels :: SDRS -> [CDU] -> SDRS
removeRels s []        = s
removeRels s (cdu:rest) = removeRels (removeRel s cdu) rest

---------------------------------------------------------------------------
-- | removes a given 'CDU' @r@ from a given SDRS @s@ iff @r@ is a
-- Relation.
---------------------------------------------------------------------------
removeRel :: SDRS -> CDU -> SDRS
removeRel (SDRS m l) r@(Relation {}) = SDRS (M.map (removeRelFromSF r) m) l
removeRel s _                        = s

---------------------------------------------------------------------------
-- | Within an 'SDRS' @s@ replaces all references of a given 'DisVar'
-- @old@ with @new@ iff @old@ occurs as a right argument of a relation.
---------------------------------------------------------------------------
updateRightArgs :: SDRS -> DisVar -> DisVar -> SDRS
updateRightArgs (SDRS m l) old new = SDRS (M.map updateR m) l
  where updateR :: SDRSFormula -> SDRSFormula
        updateR edu@(EDU {}) = edu
        updateR (CDU cdu) = CDU $ updateCDU cdu
        updateCDU :: CDU -> CDU
        updateCDU r@(Relation rel dv1 dv2)
          | dv2 == old           = Relation rel dv1 new
          | otherwise            = r
        updateCDU (And sf1 sf2)    = And (updateCDU sf1) (updateCDU sf2)
        updateCDU (Not sf1)        = Not (updateCDU sf1)

---------------------------------------------------------------------------
-- | Given a conjunction of 'SDRSFormula'e @sf@, removes the subrelation @r@
-- from the conjunction.
---------------------------------------------------------------------------
removeRelFromSF :: CDU -> SDRSFormula -> SDRSFormula
removeRelFromSF r@(Relation {}) (CDU a@(And {})) = CDU $ recurse a
  where recurse :: CDU -> CDU
        recurse (And sf1@(And {}) sf2@(And {})) = And (recurse sf1) (recurse sf2)
        recurse (And sf1@(And {}) sf2@(Relation {}))
          | r == sf2                            = recurse sf1
          | otherwise                           = And (recurse sf1) sf2
        recurse (And sf1@(Relation {}) sf2@(And {}))
          | r == sf1                            = recurse sf2
          | otherwise                           = And sf1 (recurse sf2)
        recurse a'@(And sf1@(Relation {}) sf2@(Relation {}))
          | r == sf1                            = sf2
          | r == sf2                            = sf1
          | otherwise                           = a'
        recurse sf'                             = sf'
removeRelFromSF _ sf = sf
