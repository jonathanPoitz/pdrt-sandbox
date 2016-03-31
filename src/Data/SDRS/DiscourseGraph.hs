{- |
Module      :  Data.SDRS.DiscourseGraph
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS discourse graph
-}

module Data.SDRS.DiscourseGraph
( discourseGraph
, buildOutscopeMap
--, outscopes
--, showDGraph
--, printDGraph
) where

import Data.SDRS.DataType
import Data.SDRS.Structure (expandRecursiveFormula)
import Data.Graph (graphFromEdges', Graph, Vertex)
import qualified Data.List as List 
import qualified Data.Map as M
--import qualified Data.Set as Set

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Shows a discourse graph
---------------------------------------------------------------------------
showDGraph :: DGraph -> String
showDGraph (DGraph graph labels) = show graph ++ "\n" ++ show labels

instance Show DGraph where
  show dg = '\n' : showDGraph dg

---------------------------------------------------------------------------
-- | A discourse graph, consisting of the graph structure and the labels
-- of its edges
---------------------------------------------------------------------------
data DGraph =
  DGraph Graph [(Vertex, Vertex, EdgeLabel)]

type EdgeLabel = String

---------------------------------------------------------------------------
-- | Returns, given an SDRS, a map that from each discourse variable to 
-- those that it outscopes. If @fullMap@ is set to False, the returned list
-- only contains the discourse variables that outscope others.
---------------------------------------------------------------------------
buildOutscopeMap :: SDRS -> Bool -> (M.Map DisVar [DisVar])
buildOutscopeMap (SDRS m _) buildFullMap = M.foldlWithKey build initialMap m
  where build :: (M.Map DisVar [DisVar]) -> DisVar -> SDRSFormula -> (M.Map DisVar [DisVar])
        build acc dv0 (Relation _ dv1 dv2) = M.insertWith (List.union) dv0 (List.nub [dv1,dv2]) acc
        build acc dv0 sf@(And _ _) = buildRecursive acc dv0 $ expandRecursiveFormula sf
        build acc dv0 sf@(Not _) = buildRecursive acc dv0 $ expandRecursiveFormula sf
        build acc _ _ = acc
        buildRecursive :: (M.Map DisVar [DisVar]) -> DisVar -> [SDRSFormula] -> (M.Map DisVar [DisVar])
        -- only add Relations since only they hold discourse variables as arguments
        buildRecursive acc _ [] = acc
        buildRecursive acc dv0 ((Relation _ dv1 dv2):rest) = M.insertWith (List.union) dv0 (List.nub [dv1,dv2]) (buildRecursive acc dv0 rest)
        buildRecursive acc dv0 (_:rest) = buildRecursive acc dv0 rest
        initialMap = if buildFullMap then M.fromList $ zip (M.keys m) (repeat [])
                                     else M.empty

---------------------------------------------------------------------------
-- | Given an SDRS @s@, builds up a simple labeled Graph structure using 
-- the immediate outscopings that are in place
---------------------------------------------------------------------------
discourseGraph :: SDRS -> DGraph
discourseGraph s@(SDRS m _) = DGraph g labels
  where graph_map = map (\(x,y) -> (x,x,y)) (M.assocs (buildOutscopeMap s True))
        labels = edgeLabels (map snd (M.assocs m))
        g = fst $ graphFromEdges' graph_map
        edgeLabels :: [SDRSFormula] -> [(Vertex, Vertex, EdgeLabel)]
        edgeLabels [] = []
        edgeLabels (Segment {}:rest) = edgeLabels rest
        edgeLabels (Relation l dv1 dv2:rest) = (dv1, dv2, l):(edgeLabels rest)
        edgeLabels (And sf1 sf2:rest) = edgeLabels [sf1] ++ edgeLabels [sf2] ++ edgeLabels rest
        -- should a negated relation be put on the graph as a label?
        edgeLabels (Not sf1:rest) = edgeLabels [sf1] ++ edgeLabels rest



---------------------------------------------------------------------------
-- | Lists all DUs (represented by their labels) in an SDRS that (disregarding
-- the RFC) are accessible from the given label
---------------------------------------------------------------------------
--naiveAccessibleDRSs :: SDRS -> DisVar -> [DisVar]
--naiveAccessibleDRSs (SDRS m _) dv = reachable g dv `union` 

-----------------------------------------------------------------------------
---- | Returns, given an 'SDRS' @s@, if discourse variable @dv@ outscopes 
---- another discourse variable @dv'@.
-----------------------------------------------------------------------------
--outscopes :: SDRS -> DisVar -> DisVar -> Bool
--outscopes s dv dv' = dv' `elem` (reachable g dv)
--  where (g,_) = discourseGraph s
