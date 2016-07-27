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
  sdrsProperDRSs
, sdrsPureDRSs
, sdrsUniqueDRSRefs
, validLast
, sfsStrucIsomorphic
--, sfsSemIsomorphic
, isWellformedSDRS
) where

import qualified Data.Map as M
import Data.List
--import Debug.Trace

import Data.DRS.Properties
import Data.DRS.Merge
import Data.DRS.Structure
--import Data.DRS.Show

import Data.SDRS.DataType
import Data.SDRS.Structure
import Data.SDRS.DiscourseStructure
import Data.SDRS.Binding

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
-- | Checks if the 'SDRS' @s@ only has unique DRSRefs where, i.e.:
--
--  * no embedded 'DRS' declares 'DRSRef's that are declared in any other
-- embedded 'DRS' of @s@. 
---------------------------------------------------------------------------
sdrsUniqueDRSRefs :: SDRS -> Bool
sdrsUniqueDRSRefs s = universes == nub universes
  where universes = concat $ map drsUniverse (drss s)

---------------------------------------------------------------------------
-- | checks whether the 'SDRSFormula' pointed to by LAST is meaningful, i.e.
--  * that it is an EDU, i.e. a EDU denoting a 'DRS', and
--  * that it is part of a relation, in which it is not the left argument
-- TODO interactions with root/rf
---------------------------------------------------------------------------
validLast :: SDRS -> Bool
validLast s@(SDRS m l) 
  | M.lookup l m == Nothing = False
  | otherwise               = isSegment (m M.! l) &&
                              --any (\(CDU (Relation _ _ dv')) -> dv' == l) allRelations &&
                              not (any (\(CDU (Relation _ dv _)) -> dv == l) allRelations)
  where isSegment :: SDRSFormula -> Bool
        isSegment (EDU _) = True
        isSegment _       = False -- use case instead? but watch out for npe
        allRelations = map snd $ relations s

---------------------------------------------------------------------------
-- | Checks whether two subgraphes within the same 'SDRS's @s@ are structurally
-- isomorphic, i.e., their graph structure does not differ except for different
-- labeling of DUs. These subgraphes are determined by their root nodes,
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
        comp (EDU _) (EDU _) = True -- check here for semantic isomorphism
        comp (CDU cdu1) (CDU cdu2) = compareCDU cdu1 cdu2
        comp _ _ = False
        compareCDU :: CDU -> CDU -> Bool
        compareCDU (Relation rel1 _ _) (Relation rel2 _ _)
          | rel1 == rel2 = True
          | otherwise    = False
        compareCDU (And a1 a1') (And a2 a2') = compareCDU a1 a2 &&
                                               compareCDU a1' a2'
        compareCDU (Not n1) (Not n2) = compareCDU n1 n2
        compareCDU _ _ = False

---------------------------------------------------------------------------
-- | Checks for the well-formedness of an 'SDRS'.
---------------------------------------------------------------------------
isWellformedSDRS :: SDRS -> Bool
isWellformedSDRS s = validLast s &&
                     noSelfRefs s &&
                     sdrsPureDRSs s &&
                     sdrsProperDRSs s &&
                     allRelArgsBound s &&
                     allSegmentsBound s

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

