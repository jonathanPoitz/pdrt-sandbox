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
, isStrucIsomorphic
, isSemIsomorphic'
) where

import qualified Data.Map as M
import Data.List (nub, union)

import Data.SDRS.DataType
import Data.SDRS.Structure
import Data.SDRS.DiscourseGraph
import Data.SDRS.LambdaCalculus

import Data.DRS.Properties
import Data.DRS.Merge
import Data.DRS.Structure

---------------------------------------------------------------------------
-- | Checks, given 'SDRS' s@@, whether all its embedded 'DRS's are proper.
---------------------------------------------------------------------------
sdrsProperDRSs :: SDRS -> Bool
sdrsProperDRSs s = proper $ segments s
 where proper :: [(DisVar, SDRSFormula)] -> Bool
       proper []                    = True
       proper ((dv,Segment d):rest) = (properDRS dv d) && proper rest
       proper ((_,_):rest)          = proper rest
       properDRS :: DisVar -> DRS -> Bool
       properDRS dv d = isProperDRS ((foldl (<<+>>) (DRS [] []) accDRSs) <<+>> d) -- is merging with empty DRS the only way for this?
         where accDRSs = accessibleDRSs s dv
       
---------------------------------------------------------------------------
-- | Checks, given 'SDRS' s@@, whether all its embedded 'DRS's are pure.
---------------------------------------------------------------------------
sdrsPureDRSs :: SDRS -> Bool
sdrsPureDRSs s = pure' $ segments s
  where pure' :: [(DisVar, SDRSFormula)] -> Bool
        pure' []                    = True
        pure' ((dv,Segment d):rest) = (pureDRS dv d) && pure' rest
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
--  * that it is an EDU, i.e. a Segment denoting a 'DRS', and
--  * that it is part of a relation, in which it is not the left argument
-- TODO interactions with root/rf
---------------------------------------------------------------------------
validLast :: SDRS -> Bool
validLast s@(SDRS m l) = isSegment (m M.! l) &&
                         any (\(Relation _ _ dv') -> dv' == l) allRelations &&
                         not (any (\(Relation _ dv _) -> dv == l) allRelations) -- why doesn't "not $ any (\(Relation _ dv _) -> dv == l) allRelations" work?
  where isSegment :: SDRSFormula -> Bool
        isSegment (Segment _) = True
        isSegment _           = False -- isn't there an easier way? but idk how to pattern match on Segment when not in a function. note, this also ignores the possibility that the last node is introduced in a rec. SDRSFormula
        allRelations = map snd $ relations s

---------------------------------------------------------------------------
-- | Checks whether two 'SDRS's @s1@ and @s2@ are structural isomorphic, i.e.,
-- their graph structure does not differ except for different labeling of DUs.
---------------------------------------------------------------------------
isStrucIsomorphic :: SDRS -> SDRS -> Bool
isStrucIsomorphic s1@(SDRS m1 _) s2@(SDRS m2 _) =
  length m1 == length m2 && 
         g1 == g2_conv
  where g1 = discourseGraph s1
        g2 = discourseGraph s2
        g2_conv = discourseGraph $ sdrsAlphaConvert s2 convMap
        convMap = build (zip [root s1] (repeat Outscopes)) (zip [root s2] (repeat Outscopes)) M.empty -- Outscopes is just dummy here
          where build :: [(DisVar,SDRSRelation)] -> [(DisVar,SDRSRelation)] -> M.Map DisVar DisVar -> M.Map DisVar DisVar
                build ((dv1,rel1):rest1) ((dv2,rel2):rest2) cm
                  | ((M.lookup dv1 g1) == Nothing) &&
                    ((M.lookup dv2 g2) == Nothing) &&
                    rel1 == rel2 = M.insert dv2 dv1 (build rest1 rest2 cm)
                  | ((M.lookup dv1 g1) == Nothing) &&
                    ((M.lookup dv2 g2) == Nothing) &&
                    rel1 /= rel2 = cm 
                    -- ^ if both are leaves, but their relation is different, abort this branch of recursion
                  | ((M.lookup dv1 g1) == Nothing) ||
                    ((M.lookup dv2 g2) == Nothing) = cm -- if only one of both nodes is a leaf, abort this branch of the recursion
                  | rel1 == rel2 = M.insert dv2 dv1 (build (rest1 `union` (g1 M.! dv1))
                                                           (rest2 `union` (g2 M.! dv2))
                                                           cm)
                build _ _ cm = cm -- if one SDRS is finished, return the map? 

---------------------------------------------------------------------------
-- | Checks whether two 'SDRS's @s1@ and @s2@ are strictly semantically 
-- isomorphic, i.e., they do not differ except for different labeling of DUs.
-- TODO!
---------------------------------------------------------------------------
isSemIsomorphic' :: SDRS -> SDRS -> Bool
isSemIsomorphic' s1@(SDRS m1 _) s2@(SDRS m2 _) =
  length m1 == length m2 && 
         g1 == g2_conv
  where g1 = discourseGraph s1
        g2 = discourseGraph s2
        g2_conv = discourseGraph $ sdrsAlphaConvert s2 convMap
        convMap = build (zip [root s1] (repeat Outscopes)) (zip [root s2] (repeat Outscopes)) M.empty -- Outscopes is just dummy here
          where build :: [(DisVar,SDRSRelation)] -> [(DisVar,SDRSRelation)] -> M.Map DisVar DisVar -> M.Map DisVar DisVar
                build ((dv1,rel1):rest1) ((dv2,rel2):rest2) cm
                  | ((M.lookup dv1 g1) == Nothing) &&
                    ((M.lookup dv2 g2) == Nothing) &&
                    rel1 == rel2 = M.insert dv2 dv1 (build rest1 rest2 cm)
                  | ((M.lookup dv1 g1) == Nothing) &&
                    ((M.lookup dv2 g2) == Nothing) &&
                    rel1 /= rel2 = cm 
                    -- ^ if both are leaves, but their relation is different, abort this branch of recursion
                  | ((M.lookup dv1 g1) == Nothing) ||
                    ((M.lookup dv2 g2) == Nothing) = cm -- if only one of both nodes is a leaf, abort this branch of the recursion
                  | rel1 == rel2 = M.insert dv2 dv1 (build (rest1 `union` (g1 M.! dv1))
                                                           (rest2 `union` (g2 M.! dv2))
                                                           cm)
                build _ _ cm = cm -- if one SDRS is finished, return the map? 

---------------------------------------------------------------------------
-- | Checks whether all Segments are attached to a node that's on the RF of
-- the graph before adding this Segment. It'll be difficult to check that
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

