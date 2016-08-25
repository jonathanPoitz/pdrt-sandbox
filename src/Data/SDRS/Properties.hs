{- |
Module      :  Data.SDRS.Properties
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS properties
-}

module Data.SDRS.Properties
(
-- allNodesOnRF
  isInterpretableSDRS
, sdrsProperDRSs
, sdrsPureDRSs
, sdrsUniqueDRSRefs
, sfsStrucIsomorphic
) where

import qualified Data.Map as M
import Data.List

import Data.DRS.Properties
import Data.DRS.Merge
import Data.DRS.Structure

import Data.SDRS.DataType
import Data.SDRS.Structure
import Data.SDRS.DiscourseStructure
import Data.SDRS.Binding

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Checks for the interpretability of an 'SDRS' by wrapping multiple tests of
-- binding and validity:
--   1. embedded DRSs are pure, i.e., they don't contain otiose declarations of
--      discourse referents
--   2. embedded DRSs are proper, i.e., they don't contain free discourse referents
--   3. all discourse variables in the SDRS are bound, i.e., all references to them
--      from relations are actual discourse units
--   4. valid LAST node, i.e., LAST is an EDU and occurs only at the end of relations
--   5. no self-referencing relations, such as 3:Narration(4,4) or 3:Narration(3,4)
--   6. all EDUs rhetorically connected to the SDRS (an SDRS can only be coherent if
--      it represents a fully-connected discourse graph, i.e., all embedded DRSs are
--      part of a relation)
---------------------------------------------------------------------------
isInterpretableSDRS :: SDRS -> Bool
isInterpretableSDRS s = validLast s && -- LAST is EDU and occurs only at the end of relations
                        noSelfRefs s && -- there are no circular relations
                        sdrsPureDRSs s && -- all embedded DRSs are pure (no otiose declarations of DRSRefs)
                        sdrsProperDRSs s && -- all embedded DRSs are proper (no free DRSRefs)
                        allRelArgsBound s && -- all arguments of relations exist as actual EDUs
                        allEDUsConnected s -- all EDUs are part of at least one relation

---------------------------------------------------------------------------
-- | Checks whether all Segments are attached to a node that's on the RF of
-- the graph before adding this EDU. It'll be difficult to check that
-- directly (to go to each node and to check whether the node that it was
-- added to on the RF of the graph before having added that node). Maybe
-- the problem is translatable in some way?
-- this could be something like:
--  * not two crd relations with differing end nodes can be attached to the same
--    begin nodes
--  * nothing prior to the node that's on the right side of a crd relation
--    can be attached to nodes being dominated by the node on the left side
--    of the crd rel
---------------------------------------------------------------------------
--allNodesOnRF :: SDRS -> Bool
--allNodesOnRF :: 

---------------------------------------------------------------------------
-- | Checks, given 'SDRS' @s@, whether each of its embedded 'DRS's @d@ is /proper/, where:
--
-- ['DRS' @d@ is proper /iff/]
--
--  * @d@ does not contain any free variables.
---------------------------------------------------------------------------
sdrsProperDRSs :: SDRS -> Bool
sdrsProperDRSs s = proper $ segments s
 where proper :: [(DisVar, SDRSFormula)] -> Bool
       proper []                    = True
       proper ((dv,EDU d):rest)     = (properDRS dv d) && proper rest
       proper ((_,_):rest)          = proper rest
       properDRS :: DisVar -> DRS -> Bool
       properDRS dv d = isProperDRS ((foldl (<<+>>) (DRS [] []) revAccDRSs) <<+>> d)
         where revAccDRSs = reverse $ accessibleDRSs s dv -- needs to be reversed to get correct order of merges
       
---------------------------------------------------------------------------
-- | Checks, given 'SDRS' s@@, whether each of its embedded 'DRS's @d@ is /pure/, where:
--
-- ['DRS' @d@ is pure /iff/]
--
--  * @d@ does not contain any otiose declarations of discourse referents
--    (i.e., @d@ does not contain any unbound, duplicate uses of referents).
---------------------------------------------------------------------------
sdrsPureDRSs :: SDRS -> Bool
sdrsPureDRSs s = pure' $ segments s
  where pure' :: [(DisVar, SDRSFormula)] -> Bool
        pure' []                    = True
        pure' ((dv,EDU d):rest)     = (pureDRS dv d) && pure' rest
        pure' ((_,_):rest)          = pure' rest
        pureDRS :: DisVar -> DRS -> Bool
        pureDRS dv d = (nub accUs) == accUs
          where accUs = (concat $ map drsUniverse $ accessibleDRSs s dv) ++ (drsUniverse d)

---------------------------------------------------------------------------
-- | Checks if the 'SDRS' @s@ only has unique 'DRSRef's, i.e.:
--
--  * no embedded 'DRS' declares 'DRSRef's that are declared in any other
-- embedded 'DRS' of @s@. 
---------------------------------------------------------------------------
sdrsUniqueDRSRefs :: SDRS -> Bool
sdrsUniqueDRSRefs s = universes == nub universes
  where universes = concat $ map drsUniverse (drss s)

---------------------------------------------------------------------------
-- | Checks whether two subgraphs within the same 'SDRS' @s@ are structurally
-- isomorphic, i.e., their graph structure does not differ except for different
-- labeling of DUs. These subgraphs are determined by their root nodes,
-- two 'DisVar's @dv1@ and @dv2@ that are passed along to this function as well. 
---------------------------------------------------------------------------
sfsStrucIsomorphic :: SDRS -> DisVar -> DisVar -> Bool
sfsStrucIsomorphic (SDRS m _) dv1 dv2
  | M.lookup dv1 m == Nothing ||
    M.lookup dv2 m == Nothing = False
  | otherwise                  = comp sf1 sf2
  where sf1 = m M.! dv1
        sf2 = m M.! dv2
        comp :: SDRSFormula -> SDRSFormula -> Bool
        comp (EDU _) (EDU _)       = True -- check here for semantic isomorphism
        comp (CDU cdu1) (CDU cdu2) = compareCDU cdu1 cdu2
        comp _ _                   = False
        compareCDU :: CDU -> CDU -> Bool
        compareCDU (Relation rel1 _ _) (Relation rel2 _ _)
          | rel1 == rel2 = True
          | otherwise    = False
        compareCDU (And a1 a1') (And a2 a2')
                         = compareCDU a1 a2 && compareCDU a1' a2'
        compareCDU (Not n1) (Not n2)
                         = compareCDU n1 n2
        compareCDU _ _   = False

---------------------------------------------------------------------------
-- * Private
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | checks whether the 'SDRSFormula' pointed to by LAST is meaningful, i.e.
--  * that it is an EDU, i.e. a EDU denoting a 'DRS', and
--  * that it is part of a relation, in which it is not the left argument
---------------------------------------------------------------------------
validLast :: SDRS -> Bool
validLast s@(SDRS m l) 
  | M.lookup l m == Nothing = False
  | otherwise               = isEDU (m M.! l) &&
                              any (\(CDU (Relation _ _ dv')) -> dv' == l) allRelations &&
                              not (any (\(CDU (Relation _ dv _)) -> dv == l) allRelations)
  where isEDU :: SDRSFormula -> Bool
        isEDU (EDU _) = True
        isEDU _       = False -- use case instead? but watch out for npe
        allRelations = map snd $ relations s

